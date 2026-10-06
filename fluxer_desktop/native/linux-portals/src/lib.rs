// SPDX-License-Identifier: AGPL-3.0-or-later

pub mod background;
pub mod env;
pub mod filechooser;
pub mod global_shortcuts;
pub mod gnome_shell;
pub mod kwin;
#[cfg(target_os = "linux")]
pub mod portal;
pub mod session_monitor;
pub mod settings;
pub mod x11;

#[cfg(target_os = "linux")]
pub use napi_bindings::*;

#[cfg(target_os = "linux")]
mod napi_bindings {
    use std::sync::Arc;

    use napi::{
        Env, JsDeferred, Status,
        bindgen_prelude::{Array, AsyncTask, Function, Object, Result, Task, ToNapiValue},
        sys,
        threadsafe_function::{ThreadsafeFunction, ThreadsafeFunctionCallMode, UnknownReturnValue},
    };
    use napi_derive::napi;

    use crate::{
        background::{self, RequestOptions, RequestResult},
        filechooser::{self, FileChooserResult, Filter, FilterRule, Mode, Options},
        global_shortcuts::{
            BindOutcome, DEFAULT_SESSION_TOKEN, Desktop, GlobalShortcutsClient, OpenResult,
            PortalEvent, PortalOptions, Responder, ShortcutBinding, ShortcutDefinition,
            is_valid_token, validate_definitions,
        },
        gnome_shell, kwin,
        session_monitor::{MonitorEvent, SessionMonitor},
        settings::{self, ChangeEvent, ChangePayload, ColorScheme, Contrast},
        x11,
    };

    const SETTINGS_EVENT_QUEUE_LIMIT: usize = 128;

    fn generic_error(reason: impl Into<String>) -> napi::Error {
        napi::Error::new(Status::GenericFailure, reason.into())
    }

    fn invalid_arg(reason: impl Into<String>) -> napi::Error {
        napi::Error::new(Status::InvalidArg, reason.into())
    }

    fn read_string_field(object: &Object, key: &str) -> Option<String> {
        object.get::<String>(key).ok().flatten()
    }

    fn read_string_field_or_empty(object: &Object, key: &str) -> String {
        read_string_field(object, key).unwrap_or_default()
    }

    fn read_bool_field(object: &Object, key: &str) -> Option<bool> {
        object.get::<bool>(key).ok().flatten()
    }

    fn read_object_field<'a>(object: &Object<'a>, key: &str) -> Option<Object<'a>> {
        object.get::<Object>(key).ok().flatten()
    }

    fn read_array_field<'a>(object: &Object<'a>, key: &str) -> Option<Array<'a>> {
        object.get::<Array>(key).ok().flatten()
    }

    pub struct ResolveKwinTask {
        token: String,
    }

    impl Task for ResolveKwinTask {
        type Output = Option<u32>;
        type JsValue = Option<u32>;

        fn compute(&mut self) -> Result<Self::Output> {
            kwin::resolve_kwin_window_pid(&self.token)
                .map_err(|err| generic_error(format!("resolveKwinWindowPid: {err}")))
        }

        fn resolve(&mut self, _env: Env, output: Self::Output) -> Result<Self::JsValue> {
            Ok(output)
        }
    }

    #[napi(js_name = "resolveKwinWindowPid")]
    pub fn resolve_kwin_window_pid(token: String) -> Result<AsyncTask<ResolveKwinTask>> {
        Ok(AsyncTask::new(ResolveKwinTask { token }))
    }

    pub struct ResolveX11Task {
        token: String,
    }

    impl Task for ResolveX11Task {
        type Output = Option<u32>;
        type JsValue = Option<u32>;

        fn compute(&mut self) -> Result<Self::Output> {
            x11::resolve_x11_window_pid(&self.token)
                .map_err(|err| generic_error(format!("resolveX11WindowPid: {err}")))
        }

        fn resolve(&mut self, _env: Env, output: Self::Output) -> Result<Self::JsValue> {
            Ok(output)
        }
    }

    #[napi(js_name = "resolveX11WindowPid")]
    pub fn resolve_x11_window_pid(token: String) -> Result<AsyncTask<ResolveX11Task>> {
        Ok(AsyncTask::new(ResolveX11Task { token }))
    }

    pub struct ResolveWindowPidTask {
        token: String,
    }

    impl Task for ResolveWindowPidTask {
        type Output = Option<u32>;
        type JsValue = Option<u32>;

        fn compute(&mut self) -> Result<Self::Output> {
            gnome_shell::resolve_gnome_shell_window_pid(&self.token).map_err(generic_error)
        }

        fn resolve(&mut self, _env: Env, output: Self::Output) -> Result<Self::JsValue> {
            Ok(output)
        }
    }

    #[napi(js_name = "resolveWindowPid")]
    pub fn resolve_window_pid(spec: Object) -> Result<AsyncTask<ResolveWindowPidTask>> {
        let backend = read_string_field(&spec, "backend")
            .ok_or_else(|| invalid_arg("spec.backend must be a string"))?;
        if backend != "gnome-shell-eval" {
            return Err(invalid_arg("spec.backend must be 'gnome-shell-eval'"));
        }
        let token = read_string_field(&spec, "token")
            .ok_or_else(|| invalid_arg("spec.token must be a string"))?;
        Ok(AsyncTask::new(ResolveWindowPidTask { token }))
    }

    fn parse_filter_rule(object: &Object) -> Result<FilterRule> {
        let kind = object
            .get::<u32>("kind")
            .map_err(|err| invalid_arg(err.reason.clone()))?
            .ok_or_else(|| invalid_arg("rule.kind must be a number"))?;
        if kind > 1 {
            return Err(invalid_arg("rule.kind must be 0 (glob) or 1 (mime-type)"));
        }
        let pattern = read_string_field(object, "pattern")
            .ok_or_else(|| invalid_arg("rule.pattern must be a string"))?;
        Ok(FilterRule { kind, pattern })
    }

    fn parse_filter(object: &Object) -> Result<Filter> {
        let name = read_string_field(object, "name")
            .ok_or_else(|| invalid_arg("filter.name must be a string"))?;
        let rules_array = read_array_field(object, "rules")
            .ok_or_else(|| invalid_arg("filter.rules must be an array"))?;
        let mut rules = Vec::with_capacity(rules_array.len() as usize);
        for i in 0..rules_array.len() {
            let rule_obj = rules_array
                .get::<Object>(i)
                .map_err(|err| invalid_arg(err.reason.clone()))?
                .ok_or_else(|| invalid_arg("rule must be an object"))?;
            rules.push(parse_filter_rule(&rule_obj)?);
        }
        Ok(Filter { name, rules })
    }

    fn parse_filechooser_options(object: &Object) -> Result<Options> {
        let parent_window = read_string_field_or_empty(object, "parentWindow");
        let title = read_string_field_or_empty(object, "title");
        let accept_label = read_string_field(object, "acceptLabel");
        let modal = read_bool_field(object, "modal").unwrap_or(true);
        let multiple = read_bool_field(object, "multiple").unwrap_or(false);
        let directory = read_bool_field(object, "directory").unwrap_or(false);
        let current_folder = read_string_field(object, "currentFolder");
        let current_name = read_string_field(object, "currentName");
        let current_file = read_string_field(object, "currentFile");
        let filters = if let Some(array) = read_array_field(object, "filters") {
            let mut out = Vec::with_capacity(array.len() as usize);
            for i in 0..array.len() {
                let f_obj = array
                    .get::<Object>(i)
                    .map_err(|err| invalid_arg(err.reason.clone()))?
                    .ok_or_else(|| invalid_arg("filter must be an object"))?;
                out.push(parse_filter(&f_obj)?);
            }
            out
        } else {
            Vec::new()
        };
        let current_filter = if let Some(obj) = read_object_field(object, "currentFilter") {
            Some(parse_filter(&obj)?)
        } else {
            None
        };
        Ok(Options {
            parent_window,
            title,
            accept_label,
            modal,
            multiple,
            directory,
            current_folder,
            current_name,
            current_file,
            filters,
            current_filter,
        })
    }

    pub struct FileChooserTask {
        mode: Mode,
        options: Options,
    }

    impl Task for FileChooserTask {
        type Output = FileChooserResult;
        type JsValue = Object<'static>;

        fn compute(&mut self) -> Result<Self::Output> {
            filechooser::invoke(self.mode, self.options.clone())
                .map_err(|err| generic_error(format!("FileChooser portal: {err}")))
        }

        fn resolve(&mut self, env: Env, output: Self::Output) -> Result<Self::JsValue> {
            let mut obj = Object::new(&env)?;
            obj.set("cancelled", output.cancelled)?;
            let mut array = env.create_array(output.uris.len() as u32)?;
            for (i, uri) in output.uris.iter().enumerate() {
                array.set(i as u32, uri.as_str())?;
            }
            obj.set("uris", array)?;
            Ok(unsafe { std::mem::transmute::<Object<'_>, Object<'static>>(obj) })
        }
    }

    #[napi(js_name = "openFile")]
    pub fn open_file(options: Object) -> Result<AsyncTask<FileChooserTask>> {
        let parsed = parse_filechooser_options(&options)?;
        Ok(AsyncTask::new(FileChooserTask {
            mode: Mode::Open,
            options: parsed,
        }))
    }

    #[napi(js_name = "saveFile")]
    pub fn save_file(options: Object) -> Result<AsyncTask<FileChooserTask>> {
        let parsed = parse_filechooser_options(&options)?;
        Ok(AsyncTask::new(FileChooserTask {
            mode: Mode::Save,
            options: parsed,
        }))
    }

    fn parse_string_array_field(object: &Object, key: &str) -> Result<Vec<String>> {
        let Some(array) = read_array_field(object, key) else {
            return Ok(Vec::new());
        };
        let mut out = Vec::with_capacity(array.len() as usize);
        for i in 0..array.len() {
            let value = array
                .get::<String>(i)
                .map_err(|err| invalid_arg(err.reason.clone()))?
                .ok_or_else(|| invalid_arg(format!("{key} entries must be strings")))?;
            out.push(value);
        }
        Ok(out)
    }

    fn parse_background_options(object: &Object) -> Result<RequestOptions> {
        Ok(RequestOptions {
            reason: read_string_field(object, "reason"),
            autostart: read_bool_field(object, "autostart").unwrap_or(false),
            commandline: parse_string_array_field(object, "commandline")?,
            dbus_activatable: read_bool_field(object, "dbusActivatable").unwrap_or(false),
        })
    }

    pub struct BackgroundTask {
        options: RequestOptions,
    }

    impl Task for BackgroundTask {
        type Output = RequestResult;
        type JsValue = Object<'static>;

        fn compute(&mut self) -> Result<Self::Output> {
            background::request_background(self.options.clone())
                .map_err(|err| generic_error(format!("Background portal: {err}")))
        }

        fn resolve(&mut self, env: Env, output: Self::Output) -> Result<Self::JsValue> {
            let mut obj = Object::new(&env)?;
            obj.set("response", output.response)?;
            obj.set("cancelled", output.cancelled())?;
            obj.set("background", output.background)?;
            obj.set("autostart", output.autostart)?;
            Ok(unsafe { std::mem::transmute::<Object<'_>, Object<'static>>(obj) })
        }
    }

    #[napi(js_name = "requestBackground")]
    pub fn request_background_js(options: Object) -> Result<AsyncTask<BackgroundTask>> {
        let parsed = parse_background_options(&options)?;
        Ok(AsyncTask::new(BackgroundTask { options: parsed }))
    }

    fn parse_shortcut_definitions(array: Array) -> Result<Vec<ShortcutDefinition>> {
        let mut shortcuts = Vec::with_capacity(array.len() as usize);
        for i in 0..array.len() {
            let object = array
                .get::<Object>(i)
                .map_err(|err| invalid_arg(err.reason.clone()))?
                .ok_or_else(|| invalid_arg("shortcuts must be objects"))?;
            let id = read_string_field(&object, "id")
                .ok_or_else(|| invalid_arg("shortcut.id must be a string"))?;
            let description = read_string_field(&object, "description")
                .ok_or_else(|| invalid_arg("shortcut.description must be a string"))?;
            shortcuts.push(ShortcutDefinition {
                id,
                description,
                preferred_trigger: read_string_field(&object, "preferredTrigger"),
            });
        }
        validate_definitions(&shortcuts).map_err(|err| invalid_arg(err.message()))?;
        Ok(shortcuts)
    }

    fn bindings_to_array(env: &Env, shortcuts: &[ShortcutBinding]) -> Result<Array<'static>> {
        let mut array = env.create_array(shortcuts.len() as u32)?;
        for (i, shortcut) in shortcuts.iter().enumerate() {
            let mut obj = Object::new(env)?;
            obj.set("id", shortcut.id.as_str())?;
            obj.set("description", shortcut.description.as_deref())?;
            obj.set(
                "triggerDescription",
                shortcut.trigger_description.as_deref(),
            )?;
            array.set(i as u32, obj)?;
        }
        Ok(unsafe { std::mem::transmute::<Array<'_>, Array<'static>>(array) })
    }

    impl ToNapiValue for PortalEvent {
        unsafe fn to_napi_value(raw_env: sys::napi_env, event: Self) -> Result<sys::napi_value> {
            let env = Env::from_raw(raw_env);
            let mut obj = Object::new(&env)?;
            match event {
                Self::Activated { id } => {
                    obj.set("type", "activated")?;
                    obj.set("id", id)?;
                }
                Self::Deactivated { id } => {
                    obj.set("type", "deactivated")?;
                    obj.set("id", id)?;
                }
                Self::ShortcutsChanged { shortcuts } => {
                    obj.set("type", "shortcuts-changed")?;
                    obj.set("shortcuts", bindings_to_array(&env, &shortcuts)?)?;
                }
                Self::SessionLost { reason } => {
                    obj.set("type", "session-lost")?;
                    obj.set("reason", reason.as_str())?;
                }
                Self::PortalAvailable => {
                    obj.set("type", "portal-available")?;
                }
            }
            unsafe { <Object<'_> as ToNapiValue>::to_napi_value(raw_env, obj) }
        }
    }

    impl ToNapiValue for OpenResult {
        unsafe fn to_napi_value(raw_env: sys::napi_env, result: Self) -> Result<sys::napi_value> {
            let env = Env::from_raw(raw_env);
            let mut obj = Object::new(&env)?;
            obj.set("version", result.version)?;
            obj.set("appIdSource", result.app_id_source.as_str())?;
            obj.set("uniqueName", result.unique_name)?;
            obj.set("listed", bindings_to_array(&env, &result.listed)?)?;
            unsafe { <Object<'_> as ToNapiValue>::to_napi_value(raw_env, obj) }
        }
    }

    impl ToNapiValue for BindOutcome {
        unsafe fn to_napi_value(raw_env: sys::napi_env, outcome: Self) -> Result<sys::napi_value> {
            let env = Env::from_raw(raw_env);
            let mut obj = Object::new(&env)?;
            match outcome {
                Self::Bound(shortcuts) => {
                    obj.set("outcome", "bound")?;
                    obj.set("shortcuts", bindings_to_array(&env, &shortcuts)?)?;
                }
                Self::Cancelled => obj.set("outcome", "cancelled")?,
                Self::Denied => obj.set("outcome", "denied")?,
                Self::Failed(code) => {
                    obj.set("outcome", "failed")?;
                    obj.set("code", code)?;
                }
            }
            unsafe { <Object<'_> as ToNapiValue>::to_napi_value(raw_env, obj) }
        }
    }

    type PortalEventTsfn =
        ThreadsafeFunction<PortalEvent, UnknownReturnValue, PortalEvent, Status, false, true, 0>;

    type Settle<T> = JsDeferred<T, Box<dyn FnOnce(Env) -> Result<T>>>;

    fn deferred_responder<T: ToNapiValue + 'static>(deferred: Settle<T>) -> Responder<T> {
        Responder::new(move |result| match result {
            Ok(value) => deferred.resolve(Box::new(move |_env| Ok(value))),
            Err(err) => deferred.reject(generic_error(err.to_string())),
        })
    }

    fn create_promise<T: ToNapiValue + 'static>(
        env: &Env,
    ) -> Result<(Responder<T>, Object<'static>)> {
        let (deferred, promise) = env.create_deferred::<T, Box<dyn FnOnce(Env) -> Result<T>>>()?;
        Ok((deferred_responder(deferred), unsafe {
            std::mem::transmute::<Object<'_>, Object<'static>>(promise)
        }))
    }

    #[napi]
    pub struct GlobalShortcutsPortal {
        client: GlobalShortcutsClient,
    }

    #[napi]
    impl GlobalShortcutsPortal {
        #[napi(constructor)]
        pub fn new(
            on_event: Function<PortalEvent, UnknownReturnValue>,
            options: Object,
        ) -> Result<Self> {
            let sandboxed = read_bool_field(&options, "sandboxed")
                .ok_or_else(|| invalid_arg("options.sandboxed must be a boolean"))?;
            let portal_app_id = read_string_field(&options, "portalAppId");
            let session_token = read_string_field(&options, "sessionToken")
                .unwrap_or_else(|| DEFAULT_SESSION_TOKEN.to_string());
            if !is_valid_token(&session_token) {
                return Err(invalid_arg(
                    "options.sessionToken must only contain A-Z, a-z, 0-9 and _",
                ));
            }
            let desktop = read_string_field(&options, "desktop")
                .map_or(Desktop::Other, |value| Desktop::parse(&value));
            let callback: PortalEventTsfn = on_event
                .build_threadsafe_function::<PortalEvent>()
                .weak::<true>()
                .callee_handled::<false>()
                .build()
                .map_err(|err| {
                    generic_error(format!(
                        "failed to create global shortcuts callback: {}",
                        err.reason
                    ))
                })?;
            let sink = Arc::new(move |event: PortalEvent| {
                let _ = callback.call(event, ThreadsafeFunctionCallMode::Blocking);
            });
            let client = GlobalShortcutsClient::spawn(
                PortalOptions {
                    portal_app_id,
                    sandboxed,
                    session_token,
                    desktop,
                },
                sink,
            )
            .map_err(|err| generic_error(format!("failed to start global shortcuts: {err}")))?;
            Ok(Self { client })
        }

        #[napi]
        pub fn open(&self, env: Env) -> Result<Object<'static>> {
            let (reply, promise) = create_promise::<OpenResult>(&env)?;
            self.client.open(reply);
            Ok(promise)
        }

        #[napi]
        pub fn bind(
            &self,
            env: Env,
            shortcuts: Array,
            parent_window: String,
        ) -> Result<Object<'static>> {
            let shortcuts = parse_shortcut_definitions(shortcuts)?;
            let (reply, promise) = create_promise::<BindOutcome>(&env)?;
            self.client.bind(shortcuts, parent_window, reply);
            Ok(promise)
        }

        #[napi]
        pub fn configure(&self, env: Env, parent_window: String) -> Result<Object<'static>> {
            let (reply, promise) = create_promise::<()>(&env)?;
            self.client.configure(parent_window, reply);
            Ok(promise)
        }

        #[napi]
        pub fn close(&self) {
            self.client.close();
        }
    }

    impl ToNapiValue for MonitorEvent {
        unsafe fn to_napi_value(raw_env: sys::napi_env, event: Self) -> Result<sys::napi_value> {
            let env = Env::from_raw(raw_env);
            let mut obj = Object::new(&env)?;
            obj.set(
                "type",
                match event {
                    Self::ScreenLocked => "screen-locked",
                    Self::ScreenUnlocked => "screen-unlocked",
                },
            )?;
            unsafe { <Object<'_> as ToNapiValue>::to_napi_value(raw_env, obj) }
        }
    }

    type MonitorEventTsfn =
        ThreadsafeFunction<MonitorEvent, UnknownReturnValue, MonitorEvent, Status, false, true, 0>;

    #[napi]
    pub struct SessionStateMonitor {
        monitor: SessionMonitor,
    }

    #[napi]
    impl SessionStateMonitor {
        #[napi(constructor)]
        pub fn new(on_event: Function<MonitorEvent, UnknownReturnValue>) -> Result<Self> {
            let callback: MonitorEventTsfn = on_event
                .build_threadsafe_function::<MonitorEvent>()
                .weak::<true>()
                .callee_handled::<false>()
                .build()
                .map_err(|err| {
                    generic_error(format!(
                        "failed to create session monitor callback: {}",
                        err.reason
                    ))
                })?;
            let monitor = SessionMonitor::spawn(Arc::new(move |event: MonitorEvent| {
                let _ = callback.call(event, ThreadsafeFunctionCallMode::Blocking);
            }))
            .map_err(|err| generic_error(format!("failed to start session monitor: {err}")))?;
            Ok(Self { monitor })
        }

        #[napi]
        pub fn close(&self) {
            self.monitor.close();
        }
    }

    #[napi(js_name = "readColorScheme")]
    pub fn read_color_scheme_js() -> &'static str {
        settings::read_color_scheme().as_str()
    }

    #[napi(js_name = "readContrast")]
    pub fn read_contrast_js() -> &'static str {
        settings::read_contrast().as_str()
    }

    #[napi(object)]
    pub struct AccentColorJs {
        pub r: f64,
        pub g: f64,
        pub b: f64,
    }

    #[napi(js_name = "readAccentColor")]
    pub fn read_accent_color_js() -> Option<AccentColorJs> {
        settings::read_accent_color().map(|a| AccentColorJs {
            r: a.r,
            g: a.g,
            b: a.b,
        })
    }

    pub enum NapiSettingsEvent {
        Uint32 {
            namespace: String,
            key: String,
            value: u32,
        },
        Accent {
            namespace: String,
            key: String,
            r: f64,
            g: f64,
            b: f64,
        },
        Unknown {
            namespace: String,
            key: String,
        },
    }

    impl From<ChangeEvent> for NapiSettingsEvent {
        fn from(event: ChangeEvent) -> Self {
            match event.payload {
                ChangePayload::Uint32(v) => Self::Uint32 {
                    namespace: event.namespace,
                    key: event.key,
                    value: v,
                },
                ChangePayload::Accent(a) => Self::Accent {
                    namespace: event.namespace,
                    key: event.key,
                    r: a.r,
                    g: a.g,
                    b: a.b,
                },
                ChangePayload::Unknown => Self::Unknown {
                    namespace: event.namespace,
                    key: event.key,
                },
            }
        }
    }

    impl ToNapiValue for NapiSettingsEvent {
        unsafe fn to_napi_value(raw_env: sys::napi_env, event: Self) -> Result<sys::napi_value> {
            let env = Env::from_raw(raw_env);
            let mut obj = Object::new(&env)?;
            match event {
                Self::Uint32 {
                    namespace,
                    key,
                    value,
                } => {
                    obj.set("namespace", namespace)?;
                    obj.set("key", key)?;
                    obj.set("uint32", value)?;
                }
                Self::Accent {
                    namespace,
                    key,
                    r,
                    g,
                    b,
                } => {
                    obj.set("namespace", namespace)?;
                    obj.set("key", key)?;
                    let mut accent = Object::new(&env)?;
                    accent.set("r", r)?;
                    accent.set("g", g)?;
                    accent.set("b", b)?;
                    obj.set("accent", accent)?;
                }
                Self::Unknown { namespace, key } => {
                    obj.set("namespace", namespace)?;
                    obj.set("key", key)?;
                }
            }
            unsafe { <Object<'_> as ToNapiValue>::to_napi_value(raw_env, obj) }
        }
    }

    type SettingsTsfn = Arc<
        ThreadsafeFunction<
            NapiSettingsEvent,
            UnknownReturnValue,
            NapiSettingsEvent,
            Status,
            false,
            true,
            SETTINGS_EVENT_QUEUE_LIMIT,
        >,
    >;

    #[napi]
    pub struct Settings {
        subscription: std::sync::Mutex<Option<settings::Subscription>>,
    }

    #[napi]
    impl Settings {
        #[napi(constructor)]
        pub fn new(on_change: Function<NapiSettingsEvent, UnknownReturnValue>) -> Result<Self> {
            let tsfn: SettingsTsfn = Arc::new(
                on_change
                    .build_threadsafe_function::<NapiSettingsEvent>()
                    .weak::<true>()
                    .callee_handled::<false>()
                    .max_queue_size::<SETTINGS_EVENT_QUEUE_LIMIT>()
                    .build()
                    .map_err(|err| {
                        generic_error(format!(
                            "failed to create settings callback: {}",
                            err.reason
                        ))
                    })?,
            );
            let tsfn_for_cb = tsfn.clone();
            let callback = Arc::new(move |event: ChangeEvent| {
                let _ = tsfn_for_cb.call(
                    NapiSettingsEvent::from(event),
                    ThreadsafeFunctionCallMode::NonBlocking,
                );
            });
            let sub = settings::Subscription::new(callback)
                .map_err(|err| generic_error(format!("Settings subscribe failed: {err}")))?;
            Ok(Self {
                subscription: std::sync::Mutex::new(Some(sub)),
            })
        }

        #[napi]
        pub fn close(&self) -> Result<()> {
            if let Some(sub) = self
                .subscription
                .lock()
                .map_err(|_| generic_error("settings lock poisoned"))?
                .take()
            {
                sub.close();
            }
            Ok(())
        }
    }

    impl Drop for Settings {
        fn drop(&mut self) {
            if let Ok(mut guard) = self.subscription.lock()
                && let Some(sub) = guard.take()
            {
                sub.close();
            }
        }
    }

    #[allow(dead_code)]
    fn _link_unused(_c: Contrast, _s: ColorScheme) {}
}

#[cfg(not(target_os = "linux"))]
mod napi_bindings {}
