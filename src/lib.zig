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

    var index: [64]Rgba = [1]Rgba{.{ .val = 0 }} ** 64;
    var prev: Rgba = .{ .rgba = .{} };
    var cur = prev;
    var cursor: usize = 0;
    var run_length: u8 = 0;

    const pixel_count = raw.desc.width * raw.desc.height;
    const max_size = raw.desc.width * raw.desc.height * (@intFromEnum(raw.desc.channels) + 1) +
        header_size + padding.len;
    const bytes = try allocator.alloc(u8, max_size);

    write(bytes, &cursor, magic);
    write(bytes, &cursor, raw.desc.width);
    write(bytes, &cursor, raw.desc.height);
    write(bytes, &cursor, @intFromEnum(raw.desc.channels));
    write(bytes, &cursor, @intFromEnum(raw.desc.color_space));

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
                write(bytes, &cursor, ops.run | (run_length - 1));
                run_length = 0;
            }
            continue;
        }

        if (run_length > 0) {
            write(bytes, &cursor, ops.run | (run_length - 1));
            run_length = 0;
        }

        const cur_hash = cur.hash();
        if (index[cur_hash].val == cur.val) {
            write(bytes, &cursor, ops.index | cur_hash);
            continue;
        }
        index[cur_hash] = cur;

        if (cur.rgba.a != prev.rgba.a) {
            write(bytes, &cursor, ops.rgba);
            write(bytes, &cursor, cur.rgba.r);
            write(bytes, &cursor, cur.rgba.g);
            write(bytes, &cursor, cur.rgba.b);
            write(bytes, &cursor, cur.rgba.a);
            continue;
        }

        const diff_r: i8 = @bitCast(cur.rgba.r -% prev.rgba.r);
        const diff_g: i8 = @bitCast(cur.rgba.g -% prev.rgba.g);
        const diff_b: i8 = @bitCast(cur.rgba.b -% prev.rgba.b);

        if (diff_r >= -2 and diff_r < 2 and
            diff_g >= -2 and diff_g < 2 and
            diff_b >= -2 and diff_b < 2)
        {
            write(bytes, &cursor, ops.diff |
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
            write(bytes, &cursor, ops.luma |
                @as(u8, @intCast(diff_g + 32)));
            write(bytes, &cursor, (@as(u8, @intCast(diff_gr + 8)) << 4) |
                @as(u8, @intCast(diff_gb + 8)));
            continue;
        }

        write(bytes, &cursor, ops.rgb);
        write(bytes, &cursor, cur.rgba.r);
        write(bytes, &cursor, cur.rgba.g);
        write(bytes, &cursor, cur.rgba.b);
    }

    @memcpy(bytes[cursor..][0..padding.len], padding[0..]);
    cursor += padding.len;

    return bytes;
}

pub const DecodeError = error{
    InvalidFormat,
    TooFewBytes,
    InvalidInfo,
};

pub fn decode(
    allocator: Allocator,
    bytes: QoiImage,
    decoded_format: Channels,
) (DecodeError || Allocator.Error)!RawImage {
    var channels = decoded_format;
    if (channels != .auto and channels != .rgb and channels != .rgba)
        return DecodeError.InvalidFormat;

    if (bytes.len < header_size + padding.len)
        return DecodeError.TooFewBytes;

    var index: [64]Rgba = [1]Rgba{.{ .val = 0 }} ** 64;
    var cur: Rgba = .{ .rgba = .{} };
    var cursor: usize = 0;
    var run_length: u8 = 0;

    const header_magic = read(u32, bytes, &cursor);
    const header_width = read(u32, bytes, &cursor);
    const header_height = read(u32, bytes, &cursor);
    const header_channels: Channels = @enumFromInt(read(u8, bytes, &cursor));
    const header_color_space: ColorSpace = @enumFromInt(read(u8, bytes, &cursor));

    if (header_width == 0 or header_height == 0 or
        (header_channels != .rgb and header_channels != .rgba) or
        (header_color_space != .srgb and header_color_space != .linear) or
        header_magic != magic or
        header_width > max_pixels / header_height)
    {
        std.log.err(
            \\magic = {}
            \\header_magic = {}
            \\header_width = {}
            \\header_height = {}
            \\header_channels = {}
            \\header_color_space = {}
        , .{
            magic,
            header_magic,
            header_width,
            header_height,
            header_channels,
            header_color_space,
        });
        return DecodeError.InvalidInfo;
    }

    if (channels == .auto)
        channels = header_channels;

    const chunk_count = bytes.len - padding.len;
    const pixel_count = header_width * header_height;
    const pixels = try allocator.alloc(u8, pixel_count * @intFromEnum(channels));

    for (0..pixel_count) |pixel_index| {
        defer {
            const r_index = pixel_index * @intFromEnum(channels);
            pixels[r_index + 0] = cur.rgba.r;
            pixels[r_index + 1] = cur.rgba.g;
            pixels[r_index + 2] = cur.rgba.b;
            if (channels == .rgba)
                pixels[r_index + 3] = cur.rgba.a;
        }

        if (run_length > 0) {
            run_length -= 1;
            continue;
        }

        if (cursor >= chunk_count)
            continue;

        const opcode = read(u8, bytes, &cursor);
        defer index[cur.hash()] = cur;

        if (opcode == ops.rgb or opcode == ops.rgba) {
            cur.rgba.r = read(u8, bytes, &cursor);
            cur.rgba.g = read(u8, bytes, &cursor);
            cur.rgba.b = read(u8, bytes, &cursor);
            if (opcode == ops.rgba)
                cur.rgba.a = read(u8, bytes, &cursor);
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
            const extra = read(u8, bytes, &cursor);
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
        .desc = .{
            .width = header_width,
            .height = header_height,
            .channels = channels,
            .color_space = header_color_space,
        },
    };
}

fn write(
    bytes: []u8,
    cursor: *usize,
    val: anytype,
) void {
    const T = @TypeOf(val);
    std.mem.writeInt(T, bytes[cursor.*..][0..@sizeOf(T)], val, .big);
    cursor.* += @sizeOf(T);
}

fn read(
    comptime T: type,
    bytes: []const u8,
    cursor: *usize,
) T {
    defer cursor.* += @sizeOf(T);
    return std.mem.readInt(T, bytes[cursor.*..][0..@sizeOf(T)], .big);
}

// pub export fn encodeInto(
//     writer: anytype,
//     raw: RawImage,
// ) @TypeOf(writer).Error!usize {}

test {
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

            {
                const file = std.fs.cwd().createFile("failed_test.qoi", .{}) catch unreachable;
                file.writeAll(qoi) catch unreachable;
                file.close();
            }

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
