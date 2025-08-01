const std = @import("std");
const testing = std.testing;
const Allocator = std.mem.Allocator;

//

pub const Channels = enum(u8) {
    auto = 0,
    rgb = 3,
    rgba = 4,
    _,
};

pub const ColorSpace = enum(u8) {
    srgb = 0,
    linear = 1,
    _,
};

pub const Description = struct {
    width: u32,
    height: u32,
    channels: Channels,
    color_space: ColorSpace,
};

pub const RawImage = struct {
    pixels: []const u8,
    desc: Description,
};

pub const QoiImage = []const u8;

const Rgba = extern union {
    rgba: extern struct {
        r: u8 = 0,
        g: u8 = 0,
        b: u8 = 0,
        a: u8 = 255,
    },
    val: u32,

    fn hash(self: @This()) u6 {
        return @intCast((self.rgba.r *% 3 +%
            self.rgba.g *% 5 +%
            self.rgba.b *% 7 +%
            self.rgba.a *% 11) % 64);
    }
};

const header_size = 14;
const magic: u32 = @bitCast(@as(*const [4]u8, "fioq").*);
const padding = [8]u8{ 0, 0, 0, 0, 0, 0, 0, 1 };

const ops = struct {
    const mask: u8 = 0xc0;

    const run: u8 = 0xc0;
    const index: u8 = 0x00;
    const diff: u8 = 0x40;
    const luma: u8 = 0x80;
    const rgb: u8 = 0xfe;
    const rgba: u8 = 0xff;
};

pub const max_pixels = 1_000_000_000;

pub const EncodeError = error{
    ZeroSize,
    InvalidChannels,
    ImageTooBig,
};

pub fn encode(
    allocator: Allocator,
    raw: RawImage,
) (EncodeError || Allocator.Error)!QoiImage {
    if (raw.desc.width == 0 or raw.desc.height == 0)
        return EncodeError.ZeroSize;

    if (raw.desc.channels != .rgb and raw.desc.channels != .rgba)
        return EncodeError.InvalidChannels;

    if (raw.desc.width > max_pixels / raw.desc.height)
        return EncodeError.ImageTooBig;

    const max_size = raw.desc.width * raw.desc.height *
        (@intFromEnum(raw.desc.channels) + 1) + header_size + padding.len;
    const bytes = try allocator.alloc(u8, max_size);
    defer allocator.free(bytes);

    var writer = std.io.fixedBufferStream(bytes);
    encodeInto(writer.writer(), raw) catch unreachable;

    return try allocator.dupe(u8, writer.getWritten());
}

pub fn encodeInto(
    writer: anytype,
    raw: RawImage,
) (EncodeError || @TypeOf(writer).Error)!void {
    if (raw.desc.width == 0 or raw.desc.height == 0)
        return EncodeError.ZeroSize;

    if (raw.desc.channels != .rgb and raw.desc.channels != .rgba)
        return EncodeError.InvalidChannels;

    if (raw.desc.width > max_pixels / raw.desc.height)
        return EncodeError.ImageTooBig;

    var index: [64]Rgba = [1]Rgba{.{ .val = 0 }} ** 64;
    var prev: Rgba = .{ .rgba = .{} };
    var cur = prev;
    var run_length: u8 = 0;

    const pixel_count = raw.desc.width * raw.desc.height;

    try write(writer, magic);
    try write(writer, raw.desc.width);
    try write(writer, raw.desc.height);
    try write(writer, @intFromEnum(raw.desc.channels));
    try write(writer, @intFromEnum(raw.desc.color_space));

    for (0..pixel_count) |pixel_index| {
        const r_index = pixel_index * @intFromEnum(raw.desc.channels);
        cur.rgba.r = raw.pixels[r_index + 0];
        cur.rgba.g = raw.pixels[r_index + 1];
        cur.rgba.b = raw.pixels[r_index + 2];
        if (raw.desc.channels == .rgba)
            cur.rgba.a = raw.pixels[r_index + 3];
        defer prev = cur;

        if (cur.val == prev.val) {
            run_length += 1;
            if (run_length == 62 or pixel_index + 1 == pixel_count) {
                try write(writer, ops.run | (run_length - 1));
                run_length = 0;
            }
            continue;
        }

        if (run_length > 0) {
            try write(writer, ops.run | (run_length - 1));
            run_length = 0;
        }

        const cur_hash = cur.hash();
        if (index[cur_hash].val == cur.val) {
            try write(writer, ops.index | cur_hash);
            continue;
        }
        index[cur_hash] = cur;

        if (cur.rgba.a != prev.rgba.a) {
            try write(writer, ops.rgba);
            try write(writer, cur.rgba.r);
            try write(writer, cur.rgba.g);
            try write(writer, cur.rgba.b);
            try write(writer, cur.rgba.a);
            continue;
        }

        const diff_r: i8 = @bitCast(cur.rgba.r -% prev.rgba.r);
        const diff_g: i8 = @bitCast(cur.rgba.g -% prev.rgba.g);
        const diff_b: i8 = @bitCast(cur.rgba.b -% prev.rgba.b);

        if (diff_r >= -2 and diff_r < 2 and
            diff_g >= -2 and diff_g < 2 and
            diff_b >= -2 and diff_b < 2)
        {
            try write(writer, ops.diff |
                (@as(u8, @intCast(diff_r + 2)) << 4) |
                (@as(u8, @intCast(diff_g + 2)) << 2) |
                @as(u8, @intCast(diff_b + 2)));
            continue;
        }

        const diff_gr: i8 = diff_r -% diff_g;
        const diff_gb: i8 = diff_b -% diff_g;

        if (diff_gr >= -8 and diff_gr < 8 and
            diff_g >= -32 and diff_g < 32 and
            diff_gb >= -8 and diff_gb < 8)
        {
            try write(writer, ops.luma |
                @as(u8, @intCast(diff_g + 32)));
            try write(writer, (@as(u8, @intCast(diff_gr + 8)) << 4) |
                @as(u8, @intCast(diff_gb + 8)));
            continue;
        }

        try write(writer, ops.rgb);
        try write(writer, cur.rgba.r);
        try write(writer, cur.rgba.g);
        try write(writer, cur.rgba.b);
    }

    try writer.writeAll(&padding);
}

pub const DecodeError = error{
    InvalidFormat,
    TooFewBytes,
    InvalidInfo,
    InvalidMagic,
};

pub fn decode(
    allocator: Allocator,
    bytes: QoiImage,
    decoded_format: Channels,
) (DecodeError || Allocator.Error)!RawImage {
    var reader = std.io.fixedBufferStream(bytes);
    return decodeFrom(allocator, reader.reader(), decoded_format);
}

pub fn decodeFrom(
    allocator: Allocator,
    reader: anytype,
    decoded_format: Channels,
) (DecodeError || Allocator.Error)!RawImage {
    if (decoded_format != .auto and decoded_format != .rgb and decoded_format != .rgba)
        return DecodeError.InvalidFormat;

    var index: [64]Rgba = [1]Rgba{.{ .val = 0 }} ** 64;
    var cur: Rgba = .{ .rgba = .{} };
    var run_length: u8 = 0;
    var end: bool = false;

    var desc = decodeHeaderFrom(reader) orelse
        return DecodeError.InvalidMagic;

    if (desc.width == 0 or desc.height == 0 or
        (desc.channels != .rgb and desc.channels != .rgba) or
        (desc.color_space != .srgb and desc.color_space != .linear) or
        desc.width > max_pixels / desc.height)
    {
        return DecodeError.InvalidInfo;
    }

    if (decoded_format != .auto)
        desc.channels = decoded_format;

    const pixel_count = desc.width * desc.height;
    const pixels = try allocator.alloc(u8, pixel_count * @intFromEnum(desc.channels));

    for (0..pixel_count) |pixel_index| {
        defer {
            const r_index = pixel_index * @intFromEnum(desc.channels);
            pixels[r_index + 0] = cur.rgba.r;
            pixels[r_index + 1] = cur.rgba.g;
            pixels[r_index + 2] = cur.rgba.b;
            if (desc.channels == .rgba)
                pixels[r_index + 3] = cur.rgba.a;
        }

        if (run_length > 0) {
            run_length -= 1;
            continue;
        }

        if (end) continue;

        const opcode = read(u8, reader) catch {
            end = true;
            continue;
        };
        defer index[cur.hash()] = cur;

        if (opcode == ops.rgb or opcode == ops.rgba) {
            cur.rgba.r = try read(u8, reader);
            cur.rgba.g = try read(u8, reader);
            cur.rgba.b = try read(u8, reader);
            if (opcode == ops.rgba)
                cur.rgba.a = try read(u8, reader);
            continue;
        }

        if ((opcode & ops.mask) == ops.index) {
            cur = index[opcode];
            continue;
        }

        if ((opcode & ops.mask) == ops.diff) {
            cur.rgba.r +%= ((opcode >> 4) & 0x3) -% 2;
            cur.rgba.g +%= ((opcode >> 2) & 0x3) -% 2;
            cur.rgba.b +%= (opcode & 0x3) -% 2;
            continue;
        }

        if ((opcode & ops.mask) == ops.luma) {
            const extra = try read(u8, reader);
            const diff_g: u8 = (opcode & 0x3f) -% 32;
            cur.rgba.r +%= diff_g -% 8 +% ((extra >> 4) & 0xf);
            cur.rgba.g +%= diff_g;
            cur.rgba.b +%= diff_g -% 8 +% (extra & 0xf);
            continue;
        }

        if ((opcode & ops.mask) == ops.run) {
            run_length = opcode & 0x3f;
            continue;
        }
    }

    return .{
        .pixels = pixels,
        .desc = desc,
    };
}

pub fn decodeHeader(bytes: []const u8) ?Description {
    var reader = std.io.fixedBufferStream(bytes);
    return decodeHeaderFrom(reader.reader());
}

pub fn decodeHeaderFrom(reader: anytype) ?Description {
    const header_magic = try read(u32, reader);
    const header_width = try read(u32, reader);
    const header_height = try read(u32, reader);
    const header_channels: Channels = @enumFromInt(try read(u8, reader));
    const header_color_space: ColorSpace = @enumFromInt(try read(u8, reader));

    if (header_magic != magic) return null;

    return .{
        .width = header_width,
        .height = header_height,
        .channels = header_channels,
        .color_space = header_color_space,
    };
}

fn write(
    writer: anytype,
    val: anytype,
) @TypeOf(writer).Error!void {
    const T = @TypeOf(val);
    var buffer: [@sizeOf(T)]u8 = undefined;
    std.mem.writeInt(T, &buffer, val, .big);
    try writer.writeAll(&buffer);
}

fn read(
    comptime T: type,
    reader: anytype,
) @TypeOf(reader).Error!T {
    var buffer: [@sizeOf(T)]u8 = undefined;
    _ = try reader.readAll(&buffer);
    return std.mem.readInt(T, &buffer, .big);
}

fn saveTestResult(qoi: []const u8, comptime identifier: []const u8) !void {
    const file = try std.fs.cwd().createFile("failed_test_" ++ identifier ++ ".qoi", .{});
    defer file.close();
    try file.writeAll(qoi);
}

test "round trip" {
    try testing.fuzz({}, struct {
        fn testOne(_: void, input: []const u8) anyerror!void {
            if (input.len < 2) return;

            const desc: Description = .{
                .width = input[0],
                .height = input[1],
                .channels = .rgba,
                .color_space = .srgb,
            };
            const data = input[2..];

            if (desc.width == 0 or desc.height == 0) return;

            const full_data = try testing.allocator.alloc(u8, desc.width * desc.height * 4);
            defer testing.allocator.free(full_data);
            std.mem.copyForwards(u8, full_data, data[0..@min(full_data.len, data.len)]);

            const qoi = try encode(testing.allocator, .{
                .pixels = full_data,
                .desc = desc,
            });
            defer testing.allocator.free(qoi);

            errdefer saveTestResult(qoi, "round_trip") catch {};

            const round_trip_data = try decode(testing.allocator, qoi, .rgba);
            defer testing.allocator.free(round_trip_data.pixels);

            try testing.expectEqual(desc.width, round_trip_data.desc.width);
            try testing.expectEqual(desc.height, round_trip_data.desc.height);
            try testing.expectEqual(desc.channels, round_trip_data.desc.channels);
            try testing.expectEqual(desc.color_space, round_trip_data.desc.color_space);
            try testing.expectEqualSlices(u8, full_data, round_trip_data.pixels);
        }
    }.testOne, .{});
}

test "decode garbage" {
    try testing.fuzz({}, struct {
        fn testOne(_: void, _input: []const u8) anyerror!void {
            if (_input.len < 14) return;

            const input = try testing.allocator.dupe(u8, _input);
            defer testing.allocator.free(input);

            const width = input[0];
            const height = input[0];

            // write a fake qoi header to speed up fuzzing
            var writer = std.io.fixedBufferStream(input[0..]);
            try write(writer.writer(), @as(u32, magic));
            try write(writer.writer(), @as(u32, width));
            try write(writer.writer(), @as(u32, height));
            try write(writer.writer(), @as(u8, 3));
            try write(writer.writer(), @as(u8, 0));

            const img = decode(testing.allocator, input, .auto) catch {
                return;
            };
            defer testing.allocator.free(img.pixels);

            // saveTestResult(input, "garbage") catch {};
        }
    }.testOne, .{});
}
