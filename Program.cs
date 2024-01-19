using System;
using System.IO;
using System.Xml;
using System.Xml.Serialization;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{
    static int Main(string[] args)
    {
        //if (args.Length < 1)
        //{
        //    Console.Error.WriteLine("Usage: generator <registry> <result> <api_version> [<extension>] [<extension>] ...");
        //    return 1;
        //}

        var serializer = new XmlSerializer(typeof(Registry));

        Registry registry;
        using (var sr = new StreamReader(args[0]))
        {
            registry = (Registry)serializer.Deserialize(sr);

            foreach (var ext in registry.Extensions)
            {
                ext.IsExtension = true;
            }
        }

        var emptyRequires = new FeatureSetList[0];
        var emptyEnums = new Enum[0];
        var cmds = new Dictionary<string, Command>();
        //var enums = new List<Enum>();
        foreach (var feat in registry.Features)
        {
            foreach (var featReq in feat.Requires ?? emptyRequires)
            {
                if (featReq.Items == null) continue;
                foreach (var featItem in featReq.Items)
                {
                    if (featItem is CommandFeature cmdFeat)
                    {
                        var c = registry.Commands
                            .SelectMany(c => c.Items)
                            .Single(c => c.Prototype.Name == cmdFeat.Name);

                        var name = RemovePrefix(c.Prototype.Name);
                        if (cmds.ContainsKey(name)) continue;
                        cmds[name] = c;
                    }
                    //else if (featItem is EnumFeature enumFeat)
                    //{
                    //    var enumMatches = registry.Enums
                    //        .SelectMany(e => e.Items ?? emptyEnums)
                    //        .Where(e => e.Name == enumFeat.Name)
                    //        .ToArray();

                    //    var e = enumMatches[0];

                    //    if (enumMatches.Length > 1)
                    //    {
                    //        Console.Error.WriteLine($"dup enum `{e.Name}`");
                    //    }

                    //    if (enums.ContainsKey(e.Name)) continue;
                    //    enums[e.Name] = e;
                    //}
                }
            }
        }

        var file = $"gl.zig";
        using (var stream = new StreamWriter(file, false, Encoding.UTF8))
        {
            stream.WriteLine("const std = @import(\"std\");");
            stream.WriteLine("const builtin = @import(\"builtin\");");
            stream.WriteLine("const log = std.log.scoped(.gl);");
            stream.WriteLine();
            stream.WriteLine("const tracy = @import(\"tracy\");");
            stream.WriteLine();
            stream.WriteLine(preamble);

            // write enums
            stream.WriteLine();
            foreach (var enumSet in registry.Enums)
            {
                if (enumSet.Items == null) continue;
                foreach (var e in enumSet.Items)
                {
                    stream.WriteLine("pub const {0} = {1};", MakeZigIdent(RemovePrefix(e.Name)), e.Value);
                }
            }
            //foreach (var kv in enums)
            //{
            //    var e = kv.Value;
            //    stream.WriteLine("pub const {0} = {1};", MakeZigIdent(RemovePrefix(e.Name)), e.Value);
            //}

            stream.WriteLine();
            stream.WriteLine("pub const Error = error{");
            stream.WriteLine("    InvalidEnum,");
            stream.WriteLine("    InvalidValue,");
            stream.WriteLine("    InvalidOperation,");
            stream.WriteLine("    StackOverflow,");
            stream.WriteLine("    StackUnderflow,");
            stream.WriteLine("    OutOfMemory,");
            stream.WriteLine("    InvalidFramebufferOperation,");
            stream.WriteLine("    ContextLost,");
            stream.WriteLine("    //TableTooLarge,");
            stream.WriteLine("    Unlisted,");
            stream.WriteLine("};");
            stream.WriteLine();
            stream.WriteLine("fn errorFromCode(err: GLenum) Error {");
            stream.WriteLine("    return switch (err) {");
            stream.WriteLine("        INVALID_ENUM => Error.InvalidEnum,");
            stream.WriteLine("        INVALID_VALUE => Error.InvalidValue,");
            stream.WriteLine("        INVALID_OPERATION => Error.InvalidOperation,");
            stream.WriteLine("        STACK_OVERFLOW => Error.StackOverflow,");
            stream.WriteLine("        STACK_UNDERFLOW => Error.StackUnderflow,");
            stream.WriteLine("        OUT_OF_MEMORY => Error.OutOfMemory,");
            stream.WriteLine("        INVALID_FRAMEBUFFER_OPERATION => Error.InvalidFramebufferOperation,");
            stream.WriteLine("        CONTEXT_LOST => Error.ContextLost,");
            stream.WriteLine("        //TABLE_TOO_LARGE => Error.TableTooLarge,");
            stream.WriteLine("        else => Error.Unlisted,");
            stream.WriteLine("    };");
            stream.WriteLine("}");

            // Options
            stream.WriteLine();
            stream.WriteLine("pub const CommandFlags = struct {");
            foreach (var kv in cmds)
            {
                if (kv.Key == "getError")
                {
                    // to functions that are required by default
                    stream.WriteLine("    {0}: bool = true,", kv.Key);
                }
                else
                {
                    stream.WriteLine("    {0}: bool = false,", kv.Key);
                }
            }
            stream.WriteLine("pub fn CmdType(comptime tag: std.meta.FieldEnum(CommandFlags)) type {");
            stream.WriteLine("    return switch (tag) {");
            foreach (var kv in cmds)
            {
                stream.WriteLine("    .{0} => *const {1},", kv.Key, kv.Value.GetSignature(SigName.Ptr));
            }
            stream.WriteLine("    };");
            stream.WriteLine("}");
            stream.WriteLine("pub fn cmdName(tag: std.meta.FieldEnum(CommandFlags)) [:0]const u8 {");
            stream.WriteLine("    return switch (tag) {");
            foreach (var kv in cmds)
            {
                stream.WriteLine("    .{0} => \"{1}\",", kv.Key, kv.Value.Prototype.Name);
            }
            stream.WriteLine("};");
            stream.WriteLine("}");
            stream.WriteLine(@"pub fn merge(lhs: CommandFlags, rhs: CommandFlags) CommandFlags {
    @setEvalBranchQuota(10_000);
    var result: CommandFlags = .{};
    inline for (@typeInfo(CommandFlags).Struct.fields) |field| {
        @field(result, field.name) = @field(lhs, field.name) or @field(rhs, field.name);
    }
    return result;
}
pub fn intersect(lhs: CommandFlags, rhs: CommandFlags) CommandFlags {
    @setEvalBranchQuota(10_000);
    var result: CommandFlags = .{};
    inline for (@typeInfo(CommandFlags).Struct.fields) |field| {
        @field(result, field.name) = @field(lhs, field.name) and @field(rhs, field.name);
    }
    return result;
}
pub fn complement(self: CommandFlags) CommandFlags {
    @setEvalBranchQuota(10_000);
    var result: CommandFlags = .{};
    inline for (@typeInfo(CommandFlags).Struct.fields) |field| {
        @field(result, field.name) = !@field(self, field.name);
    }
    return result;
}
pub fn subtract(lhs: CommandFlags, rhs: CommandFlags) CommandFlags {
    @setEvalBranchQuota(10_000);
    var result: CommandFlags = .{};
    inline for (@typeInfo(CommandFlags).Struct.fields) |field| {
        @field(result, field.name) = @field(lhs, field.name) and !@field(rhs, field.name);
    }
    return result;
}
pub fn contains(lhs: CommandFlags, rhs: CommandFlags) bool {
    @setEvalBranchQuota(10_000);
    inline for (@typeInfo(CommandFlags).Struct.fields) |field| {
        if (!@field(lhs, field.name) and @field(rhs, field.name)) {
            return false;
        }
    }
    return true;
}");
            stream.WriteLine("};");

            // start loader
            stream.WriteLine();
            stream.WriteLine("pub fn LoaderWrapper(comptime cmds: CommandFlags) type {");

            stream.WriteLine();
            stream.WriteLine("return struct {");
            stream.WriteLine("    dispatch: Dispatch,");

            stream.WriteLine();
            stream.WriteLine("    const Dispatch = blk:{");
            stream.WriteLine("    @setEvalBranchQuota(10_000);");
            stream.WriteLine("    const Type = std.builtin.Type;");
            stream.WriteLine("    const fields_len = fields_len: {");
            stream.WriteLine("        var fields_len: u32 = 0;");
            stream.WriteLine("        for (@typeInfo(CommandFlags).Struct.fields) |field| {");
            stream.WriteLine("            fields_len += @as(u32, @intCast(@intFromBool(@field(cmds, field.name))));");
            stream.WriteLine("        }");
            stream.WriteLine("        break :fields_len fields_len;");
            stream.WriteLine("    };");
            stream.WriteLine("    var fields: [fields_len]Type.StructField = undefined;");
            stream.WriteLine("    var i: usize = 0;");
            stream.WriteLine("    for (@typeInfo(CommandFlags).Struct.fields) |field| {");
            stream.WriteLine("        if (@field(cmds, field.name)) {");
            stream.WriteLine("            const field_tag = std.enums.nameCast(std.meta.FieldEnum(CommandFlags), field.name);");
            stream.WriteLine("            const PfnType = CommandFlags.CmdType(field_tag);");
            stream.WriteLine("            fields[i] = .{");
            stream.WriteLine("                .name = CommandFlags.cmdName(field_tag),");
            stream.WriteLine("                .type = PfnType,");
            stream.WriteLine("                .default_value = null,");
            stream.WriteLine("                .is_comptime = false,");
            stream.WriteLine("                .alignment = @alignOf(PfnType),");
            stream.WriteLine("            };");
            stream.WriteLine("            i += 1;");
            stream.WriteLine("        }");
            stream.WriteLine("    }");
            stream.WriteLine("    break :blk @Type(.{");
            stream.WriteLine("        .Struct = .{");
            stream.WriteLine("            .layout = .Auto,");
            stream.WriteLine("            .fields = &fields,");
            stream.WriteLine("            .decls = &[_]std.builtin.Type.Declaration{},");
            stream.WriteLine("            .is_tuple = false,");
            stream.WriteLine("        },");
            stream.WriteLine("    });");
            stream.WriteLine("    };");


            stream.WriteLine();
            stream.WriteLine("pub fn init() @This() {");
            stream.WriteLine("    var self: @This() = undefined;");
            stream.WriteLine("    @memset(std.mem.asBytes(&self), 0);");
            stream.WriteLine("    return self;");
            stream.WriteLine("}");

            stream.WriteLine();
            stream.WriteLine("pub fn load(self: *@This(), loader: anytype) error{CommandLoadFailure}!void {");
            stream.WriteLine("    const zone = tracy.ZoneN(@src(), \"gl.Loader.load\");");
            stream.WriteLine("    defer zone.End();");
            stream.WriteLine();
            stream.WriteLine("    inline for (std.meta.fields(Dispatch)) |field| {");
            stream.WriteLine("        const name = @as([*:0]const u8, @ptrCast(field.name ++ \"\\x00\"));");
            stream.WriteLine("        if (loader.getProcAddress(name)) |cmd_ptr| {");
            stream.WriteLine("            @field(self.dispatch, field.name) = @as(field.type, @ptrCast(cmd_ptr));");
            stream.WriteLine("        } else {");
            stream.WriteLine("            log.err(\"entry point `{s}` not found\", .{name});");
            stream.WriteLine("            return error.CommandLoadFailure;");
            stream.WriteLine("        }");
            stream.WriteLine("    }");
            stream.WriteLine("}");

            stream.WriteLine();
            foreach (var kv in cmds)
            {
                var cmd = kv.Value;
                stream.Write("pub inline ");
                stream.Write(cmd.GetSignature(SigName.Zig));
                stream.WriteLine(" {");

                stream.WriteLine("    const zone = tracy.ZoneN(@src(), \"{0}\");", cmd.Prototype.Name);
                stream.WriteLine("    defer zone.End();");

                stream.Write("    const result = self.dispatch.{0}(", cmd.Prototype.Name);
                if (cmd.Parameters != null)
                {
                    int i = 0;
                    foreach (var param in cmd.Parameters)
                    {
                        if (i > 0) stream.Write(", ");
                        stream.Write("_{0}", param.Name);
                        i += 1;
                    }
                }
                stream.WriteLine(");");

                if (kv.Key != "getError")
                {
                    stream.WriteLine("    if (validation and cmds.getError) {");
                    stream.WriteLine("        const err_code = self.dispatch.glGetError();");
                    stream.WriteLine("        if (err_code != NO_ERROR) return errorFromCode(err_code);");
                    //stream.WriteLine("        if (err_code != NO_ERROR) {");
                    //stream.WriteLine("            const err = errorFromCode(err_code);");
                    //stream.WriteLine("            log.err(\"`{{s}}` {{}} \", .{{ \"{0}\", err }});", cmd.Prototype.Name);
                    //stream.WriteLine("            return err;");
                    //stream.WriteLine("        }");
                    stream.WriteLine("    }");
                }

                stream.WriteLine("    return result;");

                stream.WriteLine("}");
            }
            stream.WriteLine("    };");

            stream.WriteLine("}");

            // todo: also include the lower level versions
            foreach (var feat in registry.Features)
            {
                var unique = new HashSet<string>();

                var gl = feat.API == "gl";
                var gles = feat.API.StartsWith("gles");
                var glsc = feat.API == "glsc2";

                var requires = new List<FeatureSetList>();
                foreach (var otherFeat in registry.Features)
                {
                    if (otherFeat == feat ||
                        otherFeat.EnumAPI == feat.EnumAPI ||
                        otherFeat.FloatNumber <= feat.FloatNumber)
                    {
                        requires.AddRange(otherFeat.Requires ?? emptyRequires);
                    }
                }

                stream.WriteLine();
                stream.WriteLine("pub const {0} = CommandFlags{{", feat.Name.Replace("GL_VERSION", "GL").Replace("VERSION", "").Replace("_", "").ToLowerInvariant());
                foreach (var featReq in requires)
                {
                    if (featReq.Items == null) continue;
                    foreach (var featItem in featReq.Items)
                    {
                        if (featItem is CommandFeature cmdFeat)
                        {
                            var c = registry.Commands
                                .SelectMany(c => c.Items)
                                .Single(c => c.Prototype.Name == cmdFeat.Name);

                            var name = RemovePrefix(c.Prototype.Name);
                            
                            if (unique.Contains(name)) continue;
                            unique.Add(name);

                            stream.WriteLine("    .{0} = true,", name);
                        }
                    }
                }
                stream.WriteLine("};");
            }
        }

        Process process = new Process();
        process.StartInfo.FileName = "zig";
        process.StartInfo.Arguments = $"fmt {file}";
        process.Start();
        process.WaitForExit();

        return 0;
    }

    public static string MakeZigIdent(string text)
    {
        var type_pat = new Regex(@"(f|u|i)\d+");

        if (type_pat.IsMatch(text))
            return "_" + text;

        switch (text)
        {
            case "type": return "_type";
            default:
                if (!char.IsLetter(text[0]))
                    return "@\"" + text + "\"";
                else
                    return text;
        }
    }

    public static string RemovePrefix(string text)
    {
        if (text.StartsWith("gl"))
            return text.Substring(2, 1).ToLower() + text.Substring(3);
        else if (text.StartsWith("GL_"))
            return text.Substring(3);
        else
            return text;
    }

    public static string TranslateC(string type)
    {
        type = type.Trim();

        if (type == "void")
            return "void";

        // this is left-const bullshittery, let's reverse it
        if (type.StartsWith("const"))
        {
            var rest = type.Substring(5).Trim();
            var index = rest.IndexOfAny(new char[] { '*', ' ' });
            if (index >= 0)
            {
                type = rest.Substring(0, index) + " const" + rest.Substring(index);
            }
        }

        var tokens = new List<string>();

        var pattern = new Regex("(\\w+)|\\*");
        foreach (Match pat in pattern.Matches(type))
        {
            tokens.Add(pat.Value);
        }

        var result = "";
        int i = tokens.Count;
        while (i > 0)
        {
            i -= 1;
            var tok = tokens[i];

            if (tok == "*")
            {
                if ((i > 0 && tokens[i - 1] == "void") || (i > 1 && tokens[i - 2] == "void"))
                {
                    result += "?*";
                }
                else
                {
                    result += "[*c]";
                }
            }
            else if (tok == "const")
                result += "const ";
            else
            {
                switch (tok)
                {
                    case "void": result += "anyopaque"; break;
                    case "int": result += "c_int"; break;
                    case "short": result += "c_short"; break;
                    case "long": result += "c_long"; break;
                    default:
                        result += tok;
                        break;
                }
            }
        }
        if (result == "[*c]const GLubyte")
        {
            // assume a string:
            return "?[*:0]const GLubyte";
        }
        return result;
    }

    const string preamble =
    @"pub const validation: bool = switch (builtin.mode) {
    .Debug, .ReleaseSafe => true,
    else => false,
};

pub const FunctionPointer: type = *align(@alignOf(fn (u32) callconv(.C) u32)) const anyopaque;

pub const GLenum = c_uint;
pub const GLboolean = u8;
pub const GLbitfield = c_uint;
pub const GLbyte = i8;
pub const GLubyte = u8;
pub const GLshort = i16;
pub const GLushort = u16;
pub const GLint = c_int;
pub const GLuint = c_uint;
pub const GLclampx = i32;
pub const GLsizei = c_int;
pub const GLfloat = f32;
pub const GLclampf = f32;
pub const GLdouble = f64;
pub const GLclampd = f64;
pub const GLeglClientBufferEXT = void;
pub const GLeglImageOES = void;
pub const GLchar = u8;
pub const GLcharARB = u8;

pub const GLhandleARB = if (builtin.os.tag == .macos) *anyopaque else c_uint;

pub const GLhalf = u16;
pub const GLhalfARB = u16;
pub const GLfixed = i32;
pub const GLintptr = usize;
pub const GLintptrARB = usize;
pub const GLsizeiptr = isize;
pub const GLsizeiptrARB = isize;
pub const GLint64 = i64;
pub const GLint64EXT = i64;
pub const GLuint64 = u64;
pub const GLuint64EXT = u64;

pub const GLsync = *opaque {};

pub const _cl_context = opaque {};
pub const _cl_event = opaque {};

pub const GLDEBUGPROC = *const fn (source: GLenum, _type: GLenum, id: GLuint, severity: GLenum, length: GLsizei, message: [*:0]const u8, userParam: ?*anyopaque) callconv(.C) void;
pub const GLDEBUGPROCARB = *const fn (source: GLenum, _type: GLenum, id: GLuint, severity: GLenum, length: GLsizei, message: [*:0]const u8, userParam: ?*anyopaque) callconv(.C) void;
pub const GLDEBUGPROCKHR = *const fn (source: GLenum, _type: GLenum, id: GLuint, severity: GLenum, length: GLsizei, message: [*:0]const u8, userParam: ?*anyopaque) callconv(.C) void;

pub const GLDEBUGPROCAMD = *const fn (id: GLuint, category: GLenum, severity: GLenum, length: GLsizei, message: [*:0]const u8, userParam: ?*anyopaque) callconv(.C) void;

pub const GLhalfNV = u16;
pub const GLvdpauSurfaceNV = GLintptr;
pub const GLVULKANPROCNV = *const fn () callconv(.C) void;
";
}

[XmlRoot("registry")]
public class Registry
{
    [XmlElement("comment")]
    public string Comment { get; set; }

    [XmlArray("types")]
    [XmlArrayItem("type")]
    public GLType[] Types { get; set; }

    [XmlArray("groups")]
    [XmlArrayItem("group")]
    public Group[] Groups { get; set; }

    [XmlElement("enums")]
    public EnumSet[] Enums { get; set; }

    [XmlElement("commands")]
    public CommandSet[] Commands { get; set; }

    [XmlElement("feature")]
    public Feature[] Features { get; set; }

    [XmlArray("extensions")]
    [XmlArrayItem("extension")]
    public Feature[] Extensions { get; set; }
}

public class GLType
{
    [XmlElement("name")]
    public string Name { get; set; }
}

public class Group
{
    [XmlAttribute("name")]
    public string Name { get; set; }

    [XmlElement("enum")]
    public Enum[] Items { get; set; }
}

public class Enum
{
    [XmlAttribute("name")]
    public string Name { get; set; }

    [XmlAttribute("value")]
    public string Value { get; set; }
}

public class EnumSet
{
    [XmlAttribute("namespace")]
    public string Namespace { get; set; }

    [XmlAttribute("group")]
    public string Group { get; set; }

    [XmlAttribute("type")]
    public string Type { get; set; }

    [XmlAttribute("comment")]
    public string Comment { get; set; }

    [XmlElement("enum")]
    public Enum[] Items { get; set; }

}

public class CommandSet
{
    [XmlAttribute("namespace")]
    public string Namespace { get; set; }

    [XmlElement("command")]
    public Command[] Items { get; set; }
}

public enum SigName
{
    Default,
    Zig,
    Ptr,
}

public class Command
{
    [XmlElement("proto")]
    public Parameter Prototype { get; set; }

    [XmlElement("param")]
    public Parameter[] Parameters { get; set; }

    [XmlElement("alias")]
    public Alias Alias { get; set; } = new Alias();

    public string GetSignature(SigName mode = SigName.Default)
    {
        var stream = new StringWriter();

        var full_signature = Prototype.FullText;
        var return_type = "void";
        {
            var index = full_signature.LastIndexOf(" ");
            if (index >= 0)
            {
                return_type = full_signature.Substring(0, index).Trim();
            }
        }

        int count = 0;
        switch (mode)
        {
            case SigName.Default:
                stream.Write("fn {0}(", Alias.Name ?? Prototype.Name);
                break;
            case SigName.Zig:
                count = 1;
                stream.Write("fn {0}(self: *const @This()", Program.RemovePrefix(Alias.Name ?? Prototype.Name));
                break;
            case SigName.Ptr:
                stream.Write("fn(");
                break;
            default:
                break;
        }

        if (Parameters != null)
        {
            foreach (var param in Parameters)
            {
                var name = param.Name;
                var type = param.TranslatedType;

                if (count > 0) stream.Write(", ");

                stream.Write("_{0}: {1}", name, type);

                count += 1;
            }
        }
        stream.Write(")");

        switch (mode)
        {
            case SigName.Zig:
                stream.Write(" Error!");
                break;
            case SigName.Default:
            case SigName.Ptr:
                stream.Write(" callconv(.C) ");
                break;
            default:
                break;
        }

        stream.Write(Program.TranslateC(return_type));

        return stream.ToString();
    }
}

public class Alias
{
    [XmlAttribute("name")]
    public string Name { get; set; } = null;
}

public class Parameter
{
    [XmlAttribute("group")]
    public string Group { get; set; }

    [XmlText(typeof(string))]
    [XmlAnyElement]
    public object[] Items { get; set; }

    [XmlIgnore]// [XmlElement("ptype")]
    public string Type => Items.OfType<XmlElement>().SingleOrDefault(e => e.Name == "ptype")?.InnerText;

    [XmlIgnore]// [XmlElement("name")]
    public string Name => Items.OfType<XmlElement>().SingleOrDefault(e => e.Name == "name")?.InnerText;

    public string TranslatedType
    {
        get
        {
            var type = Type ?? "";
            var full_text = FullText;
            var index = full_text.LastIndexOf(' ');
            if (index >= 0)
            {
                type = full_text.Substring(0, index);
            }
            return Program.TranslateC(type);
        }
    }

    public string FullText => string.Join<string>(" ", Items.Select(x =>
    {
        if (x is XmlElement e)
            return e.InnerText;
        else
            return x.ToString();
    }));
}

public enum API
{
    GL,
    GLES,
    GLSC,
}

public class Feature
{
    public bool IsExtension { get; set; } = false;

    [XmlAttribute("name")]
    public string Name { get; set; }

    [XmlAttribute("api")]
    public string API { get; set; }

    API? _enumAPI = null;
    public API EnumAPI
    {
        get
        {
            if (!_enumAPI.HasValue)
            {
                if (API == "gl")
                {
                    _enumAPI = global::API.GL;
                }
                else if (API.StartsWith("gles"))
                {
                    _enumAPI = global::API.GLES;
                }
                else if (API == "glsc2")
                { 
                    _enumAPI = global::API.GLSC;
                }
            }
            return _enumAPI.Value;

        }
    }

    [XmlAttribute("number")]
    public string Number { get; set; }

    float? _floatNumber = null;
    public float FloatNumber
    {
        get
        {
            if (!_floatNumber.HasValue)
            {
                _floatNumber = float.Parse(Number);
            }
            return _floatNumber.Value;
        }
    }

    [XmlAttribute("supported")]
    public string Supported { get; set; }

    [XmlElement("require")]
    public FeatureSetList[] Requires { get; set; }

    [XmlElement("remove")]
    public FeatureSetList[] Removes { get; set; }

    public IEnumerable<FeatureComponent> GetRequiredComponents(string api, string profile) => Filter(api, profile, Requires);

    public IEnumerable<FeatureComponent> GetRemovedComponents(string api, string profile) => Filter(api, profile, Removes);

    IEnumerable<FeatureComponent> Filter(string api, string profile, IEnumerable<FeatureSetList> input)
    {
        if (input == null)
            return new FeatureComponent[0];
        return input.Where(f => f.Items != null).Where(f => f.HasAPI(api) && f.HasProfile(profile)).SelectMany(f => f.Items);
    }

    public bool IsCompatibleTo(Feature other)
    {
        return (Supported ?? "").Split('|').Contains(other.API);
    }
}

public class FeatureSetList
{
    [XmlAttribute("comment")]
    public string Comment { get; set; }

    [XmlAttribute("api")]
    public string Api { get; set; }

    [XmlAttribute("profile")]
    public string Profile { get; set; }

    [XmlElement("enum", typeof(EnumFeature))]
    [XmlElement("type", typeof(TypeFeature))]
    [XmlElement("command", typeof(CommandFeature))]
    public FeatureComponent[] Items { get; set; }

    public bool HasAPI(string wanted_api)
    {
        if (wanted_api == null)
            return true;
        var this_api = Api?.ToLower();
        if (this_api == null)
            return true; // no restrictions
        else
            return this_api == wanted_api;
    }

    public bool HasProfile(string wanted_profile)
    {
        if (wanted_profile == null)
            return true;
        var this_profile = Profile?.ToLower() ?? "common";
        if (this_profile == "common")
            return true;
        else
            return this_profile == wanted_profile;
    }
}

public abstract class FeatureComponent
{
    [XmlAttribute("name")]
    public string Name { get; set; }

    public override int GetHashCode() => Name.GetHashCode();
}

public sealed class EnumFeature : FeatureComponent
{
    public override int GetHashCode() => Name.GetHashCode();
    public override bool Equals(object obj)
    {
        if (obj is EnumFeature other)
            return other.Name == this.Name;
        else
            return false;
    }
}

public sealed class TypeFeature : FeatureComponent
{
    public override int GetHashCode() => Name.GetHashCode();
    public override bool Equals(object obj)
    {
        if (obj is TypeFeature other)
            return other.Name == this.Name;
        else
            return false;
    }
}

public sealed class CommandFeature : FeatureComponent
{
    public override int GetHashCode() => Name.GetHashCode();

    public override bool Equals(object obj)
    {
        if (obj is CommandFeature other)
            return other.Name == this.Name;
        else
            return false;
    }
}
