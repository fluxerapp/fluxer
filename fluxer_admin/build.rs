// SPDX-License-Identifier: AGPL-3.0-or-later

use sha2::{Digest, Sha256};
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

const BUNDLED_FAMILIES: &[&str] = &["FluxerSans", "FluxerMono"];

const BUNDLED_WEIGHTS: &[u64] = &[400, 500, 600, 700];

const EXPECTED_FACE_COUNT: usize = 16;

fn main() {
    let manifest_dir =
        PathBuf::from(env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR missing"));
    let out_dir = PathBuf::from(env::var("OUT_DIR").expect("OUT_DIR missing"));

    println!("cargo:rerun-if-changed=src/styles/app.css");
    println!("cargo:rerun-if-changed=src/");
    println!("cargo:rerun-if-changed=openapi-admin.json");

    generate_admin_api(&manifest_dir, &out_dir);
    build_fonts(&manifest_dir, &out_dir);
    build_tailwind(&manifest_dir, &out_dir);
}

fn generate_admin_api(manifest_dir: &Path, out_dir: &Path) {
    let spec_path = manifest_dir.join("openapi-admin.json");

    if !spec_path.exists() {
        eprintln!("cargo:warning=openapi-admin.json not found, skipping API generation");
        return;
    }

    let json_str = fs::read_to_string(&spec_path).expect("failed to read openapi-admin.json");
    let mut spec: openapiv3::OpenAPI =
        serde_json::from_str(&json_str).expect("failed to parse openapi-admin.json");
    adapt_progenitor_throttled_errors(&mut spec);
    relax_guild_audit_log_schemas(&mut spec);
    relax_progenitor_schema_strictness(&mut spec);

    let mut settings = progenitor::GenerationSettings::new();
    settings.with_interface(progenitor::InterfaceStyle::Positional);
    settings.with_inner_type(
        "reqwest::header::HeaderMap"
            .parse()
            .expect("valid generated client header type"),
    );

    let mut generator = progenitor::Generator::new(&settings);
    let tokens = generator
        .generate_tokens(&spec)
        .expect("failed to generate admin API client");

    let content = relax_required_nullable_fields(&prettyplease::unparse(
        &syn::parse2::<syn::File>(tokens).expect("failed to parse generated tokens"),
    ));

    let output_path = out_dir.join("admin_api_generated.rs");
    fs::write(&output_path, content).expect("failed to write generated API code");
}

fn adapt_progenitor_throttled_errors(spec: &mut openapiv3::OpenAPI) {
    let schemas = &spec
        .components
        .as_ref()
        .expect("missing API components")
        .schemas;
    let error = serde_json::to_value(schemas.get("Error").expect("missing Error schema"))
        .expect("failed to inspect Error schema");
    let mut throttled = serde_json::to_value(
        schemas
            .get("ThrottledError")
            .expect("missing ThrottledError schema"),
    )
    .expect("failed to inspect ThrottledError schema");
    assert_eq!(
        error["additionalProperties"],
        serde_json::json!({}),
        "Progenitor error adaptation requires Error to retain all additional fields"
    );
    let properties = throttled["properties"]
        .as_object_mut()
        .expect("ThrottledError must be an object schema");
    assert_eq!(
        properties
            .remove("retry_after")
            .expect("missing retry_after")["type"],
        "number"
    );
    assert_eq!(
        properties.remove("global").expect("missing global")["type"],
        "boolean"
    );
    assert_eq!(
        throttled, error,
        "ThrottledError must extend the common Error schema"
    );

    for path in spec.paths.paths.values_mut() {
        let openapiv3::ReferenceOr::Item(path) = path else {
            panic!("Progenitor error adaptation requires inline API paths");
        };
        for operation in [
            &mut path.get,
            &mut path.put,
            &mut path.post,
            &mut path.delete,
            &mut path.options,
            &mut path.head,
            &mut path.patch,
            &mut path.trace,
        ]
        .into_iter()
        .flatten()
        {
            let Some(response) = operation
                .responses
                .responses
                .get_mut(&openapiv3::StatusCode::Code(429))
            else {
                continue;
            };
            let openapiv3::ReferenceOr::Item(response) = response else {
                panic!("Progenitor error adaptation requires inline 429 responses");
            };
            let schema = &mut response
                .content
                .get_mut("application/json")
                .expect("429 responses must return JSON")
                .schema;
            assert_eq!(
                schema,
                &Some(openapiv3::ReferenceOr::ref_(
                    "#/components/schemas/ThrottledError"
                )),
                "Progenitor only supports one error type per operation"
            );
            *schema = Some(openapiv3::ReferenceOr::ref_("#/components/schemas/Error"));
        }
    }
}

fn relax_guild_audit_log_schemas(spec: &mut openapiv3::OpenAPI) {
    let components = spec.components.as_mut().expect("missing API components");

    let entry = object_schema_mut(components, "GuildAuditLogEntryResponse");
    entry.additional_properties = None;
    let openapiv3::ReferenceOr::Item(options) = entry
        .properties
        .get_mut("options")
        .expect("GuildAuditLogEntryResponse has no options property")
    else {
        panic!("GuildAuditLogEntryResponse options must be an inline schema");
    };
    let openapiv3::SchemaKind::Type(openapiv3::Type::Object(options)) = &mut options.schema_kind
    else {
        panic!("GuildAuditLogEntryResponse options must be an object schema");
    };
    options.additional_properties = None;

    let change = object_schema_mut(components, "AuditLogChangeSchema");
    change.additional_properties = None;
    for property in ["old_value", "new_value"] {
        change.properties.insert(
            property.to_string(),
            openapiv3::ReferenceOr::Item(Box::new(openapiv3::Schema {
                schema_data: openapiv3::SchemaData::default(),
                schema_kind: openapiv3::SchemaKind::Any(openapiv3::AnySchema::default()),
            })),
        );
    }
}

fn object_schema_mut<'a>(
    components: &'a mut openapiv3::Components,
    name: &str,
) -> &'a mut openapiv3::ObjectType {
    let Some(openapiv3::ReferenceOr::Item(schema)) = components.schemas.get_mut(name) else {
        panic!("missing inline {name} schema");
    };
    let openapiv3::SchemaKind::Type(openapiv3::Type::Object(object)) = &mut schema.schema_kind
    else {
        panic!("{name} must be an object schema");
    };
    object
}

const MAX_SCHEMA_REFERENCE_DEPTH: usize = 32;

fn relax_required_nullable_fields(generated: &str) -> String {
    const PRESENCE_CHECK: &str =
        "#[serde(deserialize_with = \"::std::option::Option::deserialize\")]";
    generated
        .lines()
        .filter(|line| line.trim() != PRESENCE_CHECK)
        .flat_map(|line| [line, "\n"])
        .collect()
}

fn relax_progenitor_schema_strictness(spec: &mut openapiv3::OpenAPI) {
    let registry = spec.components.clone().unwrap_or_default();

    if let Some(components) = spec.components.as_mut() {
        for schema in components.schemas.values_mut() {
            relax_schema_reference(schema, &registry);
        }
        for response in components.responses.values_mut() {
            if let openapiv3::ReferenceOr::Item(response) = response {
                relax_response(response, &registry);
            }
        }
        for parameter in components.parameters.values_mut() {
            if let openapiv3::ReferenceOr::Item(parameter) = parameter {
                relax_parameter(parameter, &registry);
            }
        }
        for request_body in components.request_bodies.values_mut() {
            if let openapiv3::ReferenceOr::Item(request_body) = request_body {
                relax_content(&mut request_body.content, &registry);
            }
        }
        for header in components.headers.values_mut() {
            if let openapiv3::ReferenceOr::Item(header) = header {
                relax_parameter_format(&mut header.format, &registry);
            }
        }
    }

    for path in spec.paths.paths.values_mut() {
        let openapiv3::ReferenceOr::Item(path) = path else {
            continue;
        };
        for parameter in &mut path.parameters {
            if let openapiv3::ReferenceOr::Item(parameter) = parameter {
                relax_parameter(parameter, &registry);
            }
        }
        for operation in [
            &mut path.get,
            &mut path.put,
            &mut path.post,
            &mut path.delete,
            &mut path.options,
            &mut path.head,
            &mut path.patch,
            &mut path.trace,
        ]
        .into_iter()
        .flatten()
        {
            for parameter in &mut operation.parameters {
                if let openapiv3::ReferenceOr::Item(parameter) = parameter {
                    relax_parameter(parameter, &registry);
                }
            }
            if let Some(openapiv3::ReferenceOr::Item(request_body)) =
                operation.request_body.as_mut()
            {
                relax_content(&mut request_body.content, &registry);
            }
            for response in operation
                .responses
                .responses
                .values_mut()
                .chain(operation.responses.default.iter_mut())
            {
                if let openapiv3::ReferenceOr::Item(response) = response {
                    relax_response(response, &registry);
                }
            }
        }
    }
}

fn relax_response(response: &mut openapiv3::Response, registry: &openapiv3::Components) {
    relax_content(&mut response.content, registry);
    for header in response.headers.values_mut() {
        if let openapiv3::ReferenceOr::Item(header) = header {
            relax_parameter_format(&mut header.format, registry);
        }
    }
}

fn relax_content(content: &mut openapiv3::Content, registry: &openapiv3::Components) {
    for media_type in content.values_mut() {
        if let Some(schema) = media_type.schema.as_mut() {
            relax_schema_reference(schema, registry);
        }
    }
}

fn relax_parameter(parameter: &mut openapiv3::Parameter, registry: &openapiv3::Components) {
    let format = match parameter {
        openapiv3::Parameter::Query { parameter_data, .. }
        | openapiv3::Parameter::Header { parameter_data, .. }
        | openapiv3::Parameter::Path { parameter_data, .. }
        | openapiv3::Parameter::Cookie { parameter_data, .. } => &mut parameter_data.format,
    };
    relax_parameter_format(format, registry);
}

fn relax_parameter_format(
    format: &mut openapiv3::ParameterSchemaOrContent,
    registry: &openapiv3::Components,
) {
    match format {
        openapiv3::ParameterSchemaOrContent::Schema(schema) => {
            relax_schema_reference(schema, registry)
        }
        openapiv3::ParameterSchemaOrContent::Content(content) => relax_content(content, registry),
    }
}

fn relax_schema_reference(
    schema: &mut openapiv3::ReferenceOr<openapiv3::Schema>,
    registry: &openapiv3::Components,
) {
    if let openapiv3::ReferenceOr::Item(schema) = schema {
        relax_schema(schema, registry);
    }
}

fn relax_boxed_schema_reference(
    schema: &mut openapiv3::ReferenceOr<Box<openapiv3::Schema>>,
    registry: &openapiv3::Components,
) {
    if let openapiv3::ReferenceOr::Item(schema) = schema {
        relax_schema(schema, registry);
    }
}

fn relax_schema(schema: &mut openapiv3::Schema, registry: &openapiv3::Components) {
    if flattens_objects_beside_scalars(&schema.schema_kind, registry) {
        schema.schema_kind = openapiv3::SchemaKind::Any(openapiv3::AnySchema::default());
        return;
    }
    match &mut schema.schema_kind {
        openapiv3::SchemaKind::Type(openapiv3::Type::Object(object)) => {
            relax_additional_properties(&mut object.additional_properties, registry);
            for property in object.properties.values_mut() {
                relax_boxed_schema_reference(property, registry);
            }
        }
        openapiv3::SchemaKind::Type(openapiv3::Type::Array(array)) => {
            if let Some(items) = array.items.as_mut() {
                relax_boxed_schema_reference(items, registry);
            }
        }
        openapiv3::SchemaKind::Type(_) => {}
        openapiv3::SchemaKind::OneOf { one_of: subschemas }
        | openapiv3::SchemaKind::AllOf { all_of: subschemas }
        | openapiv3::SchemaKind::AnyOf { any_of: subschemas } => {
            for subschema in subschemas {
                relax_schema_reference(subschema, registry);
            }
        }
        openapiv3::SchemaKind::Not { not } => relax_schema_reference(not, registry),
        openapiv3::SchemaKind::Any(any) => {
            relax_additional_properties(&mut any.additional_properties, registry);
            for property in any.properties.values_mut() {
                relax_boxed_schema_reference(property, registry);
            }
            if let Some(items) = any.items.as_mut() {
                relax_boxed_schema_reference(items, registry);
            }
            for subschema in any
                .one_of
                .iter_mut()
                .chain(any.all_of.iter_mut())
                .chain(any.any_of.iter_mut())
            {
                relax_schema_reference(subschema, registry);
            }
            if let Some(not) = any.not.as_mut() {
                relax_schema_reference(not, registry);
            }
        }
    }
}

fn relax_additional_properties(
    additional_properties: &mut Option<openapiv3::AdditionalProperties>,
    registry: &openapiv3::Components,
) {
    match additional_properties {
        Some(openapiv3::AdditionalProperties::Any(false)) => *additional_properties = None,
        Some(openapiv3::AdditionalProperties::Schema(schema)) => {
            relax_schema_reference(schema, registry)
        }
        _ => {}
    }
}

fn flattens_objects_beside_scalars(
    schema_kind: &openapiv3::SchemaKind,
    registry: &openapiv3::Components,
) -> bool {
    let subschemas = match schema_kind {
        openapiv3::SchemaKind::OneOf { one_of } => one_of,
        openapiv3::SchemaKind::AnyOf { any_of } => any_of,
        _ => return false,
    };
    let mut objects = false;
    let mut scalars = false;
    for subschema in subschemas {
        if resolves_to_object(subschema, registry, MAX_SCHEMA_REFERENCE_DEPTH) {
            objects = true;
        } else {
            scalars = true;
        }
    }
    objects && scalars
}

fn resolves_to_object(
    schema: &openapiv3::ReferenceOr<openapiv3::Schema>,
    registry: &openapiv3::Components,
    depth: usize,
) -> bool {
    let Some(depth) = depth.checked_sub(1) else {
        return false;
    };
    let schema = match schema {
        openapiv3::ReferenceOr::Reference { reference } => {
            let Some(target) = reference
                .strip_prefix("#/components/schemas/")
                .and_then(|name| registry.schemas.get(name))
            else {
                return false;
            };
            return resolves_to_object(target, registry, depth);
        }
        openapiv3::ReferenceOr::Item(schema) => schema,
    };
    match &schema.schema_kind {
        openapiv3::SchemaKind::Type(openapiv3::Type::Object(_)) => true,
        openapiv3::SchemaKind::Type(_) => false,
        openapiv3::SchemaKind::OneOf { one_of: subschemas }
        | openapiv3::SchemaKind::AllOf { all_of: subschemas }
        | openapiv3::SchemaKind::AnyOf { any_of: subschemas } => subschemas
            .iter()
            .any(|subschema| resolves_to_object(subschema, registry, depth)),
        openapiv3::SchemaKind::Not { .. } => false,
        openapiv3::SchemaKind::Any(any) => {
            any.typ.as_deref() == Some("object")
                || !any.properties.is_empty()
                || any.additional_properties.is_some()
        }
    }
}

struct Face {
    css_family: String,
    weight: u64,
    style: String,
    source: String,
}

struct Asset {
    name: String,
    content_type: &'static str,
}

fn build_fonts(manifest_dir: &Path, out_dir: &Path) {
    println!("cargo:rerun-if-changed=../packages/fonts/manifest.json");
    println!("cargo:rerun-if-changed=../packages/fonts/files/FluxerSans");
    println!("cargo:rerun-if-changed=../packages/fonts/files/FluxerMono");
    println!("cargo:rerun-if-changed=../packages/fonts/NOTICE.md");
    println!("cargo:rerun-if-changed=../packages/fonts/LICENSE-IBM-PLEX.txt");

    let package_dir = manifest_dir.join("../packages/fonts");
    let fonts_dir = out_dir.join("static").join("fonts");
    let _ = fs::remove_dir_all(&fonts_dir);
    fs::create_dir_all(&fonts_dir).expect("failed to create generated font dir");

    let mut assets = Vec::new();
    let notice = emit_asset(
        &fonts_dir,
        &package_dir.join("NOTICE.md"),
        "text/plain; charset=utf-8",
        &mut assets,
    );
    let plex_license = emit_asset(
        &fonts_dir,
        &package_dir.join("LICENSE-IBM-PLEX.txt"),
        "text/plain; charset=utf-8",
        &mut assets,
    );

    let faces = select_faces(&package_dir);
    assert_eq!(
        faces.len(),
        EXPECTED_FACE_COUNT,
        "packages/fonts no longer offers the {} Latin-core faces fluxer_admin renders; \
         reconcile BUNDLED_FAMILIES/BUNDLED_WEIGHTS with the manifest",
        EXPECTED_FACE_COUNT
    );

    let mut stylesheet = String::from("/* SPDX-License-Identifier: AGPL-3.0-or-later */\n");
    stylesheet.push_str(
        "/* @generated by fluxer_admin/build.rs from packages/fonts. Do not edit by hand. */\n\n",
    );
    stylesheet.push_str(&format!(
        "/*\n\
         \x20* IBM Plex is licensed under the SIL Open Font License 1.1.\n\
         \x20* The IBM Plex faces below are Modified Versions renamed to \"Fluxer Sans\", so OFL\n\
         \x20* clause 3 requires the disclosure to travel with them. It is served beside them:\n\
         \x20*   ./{notice}\n\
         \x20*   ./{plex_license}\n\
         \x20*\n\
         \x20* Every url() below is relative, so it resolves against this stylesheet's own\n\
         \x20* directory. That keeps the sheet correct under any FLUXER_ADMIN_BASE_PATH without\n\
         \x20* the build having to know the runtime base path.\n\
         \x20*/\n"
    ));
    for face in &faces {
        let source = package_dir.join("files").join(&face.source);
        let file = emit_asset(&fonts_dir, &source, "font/woff2", &mut assets);
        stylesheet.push_str(&format!(
            "@font-face {{\n\
             \tfont-family: '{}';\n\
             \tsrc: url('{file}') format('woff2');\n\
             \tfont-weight: {};\n\
             \tfont-style: {};\n\
             \tfont-display: swap;\n\
             }}\n",
            face.css_family, face.weight, face.style
        ));
    }

    let stylesheet_name = write_hashed(
        &fonts_dir,
        "fonts.css",
        stylesheet.as_bytes(),
        "text/css; charset=utf-8",
        &mut assets,
    );

    write_font_asset_table(out_dir, &stylesheet_name, &assets);
}

fn select_faces(package_dir: &Path) -> Vec<Face> {
    let manifest_path = package_dir.join("manifest.json");
    let raw = fs::read_to_string(&manifest_path).unwrap_or_else(|err| {
        panic!(
            "failed to read {}: {err}. packages/fonts is generated by \
             `python3 tools/fonts/build_fonts.py`.",
            manifest_path.display()
        )
    });
    let manifest: serde_json::Value =
        serde_json::from_str(&raw).expect("failed to parse packages/fonts/manifest.json");
    let families = manifest["families"]
        .as_array()
        .expect("packages/fonts/manifest.json has no families array");

    let mut faces = Vec::new();
    for wanted in BUNDLED_FAMILIES {
        let family = families
            .iter()
            .find(|family| family["id"].as_str() == Some(wanted))
            .unwrap_or_else(|| panic!("packages/fonts/manifest.json has no family {wanted}"));
        assert_eq!(
            family["latinCore"].as_bool(),
            Some(true),
            "{wanted} is no longer a Latin-core family; it would need unicode-range gating"
        );
        let css_family = family["cssFamily"]
            .as_str()
            .unwrap_or_else(|| panic!("{wanted} has no cssFamily"))
            .to_owned();
        for face in family["faces"]
            .as_array()
            .unwrap_or_else(|| panic!("{wanted} has no faces array"))
        {
            let weight = face["weight"].as_u64().expect("face has no weight");
            if !BUNDLED_WEIGHTS.contains(&weight) {
                continue;
            }
            assert!(
                face["unicodeRange"].is_null(),
                "{wanted} face {} carries a unicode-range; Latin-core faces must not",
                face["file"]
            );
            faces.push(Face {
                css_family: css_family.clone(),
                weight,
                style: face["style"]
                    .as_str()
                    .expect("face has no style")
                    .to_owned(),
                source: face["file"].as_str().expect("face has no file").to_owned(),
            });
        }
    }
    faces
}

fn emit_asset(
    fonts_dir: &Path,
    source: &Path,
    content_type: &'static str,
    assets: &mut Vec<Asset>,
) -> String {
    let bytes =
        fs::read(source).unwrap_or_else(|err| panic!("failed to read {}: {err}", source.display()));
    let file_name = source
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or_else(|| panic!("{} has no file name", source.display()));
    write_hashed(fonts_dir, file_name, &bytes, content_type, assets)
}

fn write_hashed(
    fonts_dir: &Path,
    file_name: &str,
    bytes: &[u8],
    content_type: &'static str,
    assets: &mut Vec<Asset>,
) -> String {
    let (stem, extension) = file_name
        .rsplit_once('.')
        .unwrap_or_else(|| panic!("{file_name} has no extension to hash around"));
    let digest = Sha256::digest(bytes);
    let hash: String = digest
        .iter()
        .take(8)
        .map(|byte| format!("{byte:02x}"))
        .collect();
    let name = format!("{stem}.{hash}.{extension}");
    fs::write(fonts_dir.join(&name), bytes)
        .unwrap_or_else(|err| panic!("failed to write {name}: {err}"));
    assets.push(Asset {
        name: name.clone(),
        content_type,
    });
    name
}

fn write_font_asset_table(out_dir: &Path, stylesheet_name: &str, assets: &[Asset]) {
    let mut generated = String::from(
        "// @generated by fluxer_admin/build.rs from packages/fonts. Do not edit by hand.\n\n",
    );
    generated.push_str(&format!(
        "/// File name of the content-hashed `@font-face` stylesheet, relative to `/static/fonts/`.\npub const STYLESHEET_FILE_NAME: &str = {stylesheet_name:?};\n\n"
    ));
    generated.push_str("/// `(file name, content type, bytes)` for everything served under `/static/fonts/`.\npub static ASSETS: &[(&str, &str, &[u8])] = &[\n");
    for asset in assets {
        generated.push_str(&format!(
            "    ({:?}, {:?}, include_bytes!(concat!(env!(\"OUT_DIR\"), \"/static/fonts/{}\"))),\n",
            asset.name, asset.content_type, asset.name
        ));
    }
    generated.push_str("];\n");
    fs::write(out_dir.join("static").join("fonts.rs"), generated)
        .expect("failed to write generated font asset table");
}

fn build_tailwind(manifest_dir: &Path, out_dir: &Path) {
    let output_dir = out_dir.join("static");
    fs::create_dir_all(&output_dir).expect("failed to create generated static dir");
    let input = manifest_dir.join("src/styles/app.css");
    let output = output_dir.join("app.css");
    let candidates = [
        manifest_dir.join("node_modules/.bin/tailwindcss"),
        manifest_dir.join("../node_modules/.bin/tailwindcss"),
    ];
    let Some(cli) = candidates.iter().find(|c| c.exists()) else {
        panic!(
            "tailwindcss CLI not found. Run `pnpm install` in fluxer_admin/.\n\
             Searched: {:?}",
            candidates
        );
    };
    let status = Command::new(cli)
        .arg("-i")
        .arg(&input)
        .arg("-o")
        .arg(&output)
        .arg("--minify")
        .arg("--cwd")
        .arg(manifest_dir)
        .status()
        .expect("failed to run tailwindcss");
    if !status.success() {
        panic!("tailwindcss failed with status {}", status);
    }
}
