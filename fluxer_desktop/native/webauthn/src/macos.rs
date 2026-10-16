#![allow(non_snake_case)]

// SPDX-License-Identifier: AGPL-3.0-or-later

use std::ptr;
use std::sync::Mutex;
use std::sync::mpsc::{Sender, channel};

use dispatch2::{
    DispatchQueue, DispatchRetained, DispatchSemaphore, DispatchTime, MainThreadBound, WaitError,
};
use napi::Result;
use objc2::rc::Retained;
use objc2::runtime::{AnyClass, AnyObject, NSObject, NSObjectProtocol, ProtocolObject, Sel};
use objc2::{
    AllocAnyThread, ClassType, DefinedClass, MainThreadMarker, MainThreadOnly, define_class,
    msg_send, sel,
};
use objc2_app_kit::NSApplication;
use objc2_authentication_services::{
    ASAuthorization, ASAuthorizationController, ASAuthorizationControllerDelegate,
    ASAuthorizationControllerPresentationContextProviding,
    ASAuthorizationPlatformPublicKeyCredentialDescriptor,
    ASAuthorizationPlatformPublicKeyCredentialProvider,
    ASAuthorizationPublicKeyCredentialAssertionRequest,
    ASAuthorizationPublicKeyCredentialAttachment,
    ASAuthorizationPublicKeyCredentialAttestationKindDirect,
    ASAuthorizationPublicKeyCredentialAttestationKindIndirect,
    ASAuthorizationPublicKeyCredentialAttestationKindNone,
    ASAuthorizationPublicKeyCredentialParameters,
    ASAuthorizationPublicKeyCredentialRegistrationRequest,
    ASAuthorizationPublicKeyCredentialUserVerificationPreferenceDiscouraged,
    ASAuthorizationPublicKeyCredentialUserVerificationPreferencePreferred,
    ASAuthorizationPublicKeyCredentialUserVerificationPreferenceRequired, ASAuthorizationRequest,
    ASAuthorizationSecurityKeyPublicKeyCredentialDescriptor,
    ASAuthorizationSecurityKeyPublicKeyCredentialDescriptorTransportBluetooth,
    ASAuthorizationSecurityKeyPublicKeyCredentialDescriptorTransportNFC,
    ASAuthorizationSecurityKeyPublicKeyCredentialDescriptorTransportUSB,
    ASAuthorizationSecurityKeyPublicKeyCredentialProvider,
    ASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest, ASCOSEAlgorithmIdentifier,
    ASPresentationAnchor,
};
use objc2_foundation::{
    NSArray, NSCopying, NSData, NSError, NSMutableArray, NSNumber, NSString, NSThread,
};

use crate::common::{
    CREATE_PREFIX, CreateInput, CreateResult, DescriptorInput, GET_PREFIX, GetInput, GetResult,
    TRANSPORT_INTERNAL, TRANSPORT_USB, ceremony_error,
};

pub fn is_supported() -> bool {
    AnyClass::get(c"ASAuthorizationPlatformPublicKeyCredentialProvider").is_some()
        && AnyClass::get(c"ASAuthorizationSecurityKeyPublicKeyCredentialProvider").is_some()
}

pub fn make_credential(input: &mut CreateInput) -> Result<CreateResult> {
    if NSThread::isMainThread_class() {
        return Err(ceremony_error(
            CREATE_PREFIX,
            "WebAuthn registration must run from native async work, not the main thread",
        ));
    }
    match run_create(input) {
        Ok(out) => Ok(out),
        Err(msg) => Err(ceremony_error(CREATE_PREFIX, &msg)),
    }
}

pub fn get_assertion(input: &mut GetInput) -> Result<GetResult> {
    if NSThread::isMainThread_class() {
        return Err(ceremony_error(
            GET_PREFIX,
            "WebAuthn authentication must run from native async work, not the main thread",
        ));
    }
    match run_get(input) {
        Ok(out) => Ok(out),
        Err(msg) => Err(ceremony_error(GET_PREFIX, &msg)),
    }
}

enum CompletionPayload {
    Create {
        raw_id: Vec<u8>,
        attestation_object: Vec<u8>,
        attachment: AttachmentKind,
    },
    Get {
        raw_id: Vec<u8>,
        authenticator_data: Vec<u8>,
        signature: Vec<u8>,
        user_handle: Option<Vec<u8>>,
        attachment: AttachmentKind,
    },
    Error(String),
}

#[derive(Copy, Clone)]
enum AttachmentKind {
    Platform,
    CrossPlatform,
}

impl AttachmentKind {
    fn transport_bit(self) -> u32 {
        match self {
            AttachmentKind::Platform => TRANSPORT_INTERNAL,
            AttachmentKind::CrossPlatform => TRANSPORT_USB,
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum CeremonyKind {
    Create,
    Get,
}

struct DelegateIvars {
    semaphore: DispatchRetained<DispatchSemaphore>,
    sender: Mutex<Option<Sender<CompletionPayload>>>,
    kind: CeremonyKind,
}

define_class!(
    #[unsafe(super = NSObject)]
    #[thread_kind = MainThreadOnly]
    #[name = "FluxerWebAuthnDelegate"]
    #[ivars = DelegateIvars]
    struct WebAuthnDelegate;

    unsafe impl NSObjectProtocol for WebAuthnDelegate {}

    unsafe impl ASAuthorizationControllerDelegate for WebAuthnDelegate {
        #[unsafe(method(authorizationController:didCompleteWithAuthorization:))]

        unsafe fn did_complete_with_authorization(
            &self,
            _controller: &ASAuthorizationController,
            authorization: &ASAuthorization,
        ) {
            let credential = unsafe { authorization.credential() };
            let credential_obj: &AnyObject =
                unsafe { &*(&*credential as *const _ as *const AnyObject) };
            let payload = match self.ivars().kind {
                CeremonyKind::Create => extract_registration(credential_obj),
                CeremonyKind::Get => extract_assertion(credential_obj),
            };
            self.deliver(payload);
        }

        #[unsafe(method(authorizationController:didCompleteWithError:))]

        unsafe fn did_complete_with_error(
            &self,
            _controller: &ASAuthorizationController,
            error: &NSError,
        ) {
            self.deliver(CompletionPayload::Error(format_ns_error(error)));
        }
    }

    unsafe impl ASAuthorizationControllerPresentationContextProviding for WebAuthnDelegate {
        #[unsafe(method_id(presentationAnchorForAuthorizationController:))]
        unsafe fn presentation_anchor(
            &self,
            _controller: &ASAuthorizationController,
        ) -> Retained<ASPresentationAnchor> {
            match MainThreadMarker::new() {
                Some(mtm) => presentation_anchor(mtm),
                None => placeholder_presentation_anchor(),
            }
        }
    }
);

impl WebAuthnDelegate {
    fn new(
        mtm: MainThreadMarker,
        kind: CeremonyKind,
        sender: Sender<CompletionPayload>,
        semaphore: DispatchRetained<DispatchSemaphore>,
    ) -> Retained<Self> {
        let ivars = DelegateIvars {
            semaphore,
            sender: Mutex::new(Some(sender)),
            kind,
        };
        let this = Self::alloc(mtm).set_ivars(ivars);
        unsafe { msg_send![super(this), init] }
    }

    fn deliver(&self, payload: CompletionPayload) {
        let Ok(mut slot) = self.ivars().sender.lock() else {
            self.ivars().semaphore.signal();
            return;
        };
        if let Some(sender) = slot.take() {
            let _ = sender.send(payload);
        }
        self.ivars().semaphore.signal();
    }
}

fn presentation_anchor(mtm: MainThreadMarker) -> Retained<ASPresentationAnchor> {
    let app = NSApplication::sharedApplication(mtm);
    let key_window: Option<Retained<objc2_app_kit::NSWindow>> = app.keyWindow();
    if let Some(window) = key_window {
        return unsafe { Retained::cast_unchecked(window) };
    }
    let windows = app.windows();
    if let Some(first) = windows.firstObject() {
        return unsafe { Retained::cast_unchecked(first) };
    }

    placeholder_presentation_anchor()
}

fn placeholder_presentation_anchor() -> Retained<ASPresentationAnchor> {
    let obj = NSObject::new();

    unsafe { Retained::cast_unchecked(obj) }
}

#[derive(Default)]
struct ControllerOverrideIvars {
    client_data_hash: Option<Retained<NSData>>,
    supported_algorithms: Option<Retained<NSArray<NSNumber>>>,

    exclude_credentials: Option<Retained<NSArray<AnyObject>>>,
    require_resident_key: bool,
    is_create: bool,
}

define_class!(






    #[unsafe(super = ASAuthorizationController)]
    #[name = "FluxerWebAuthnController"]
    #[ivars = core::cell::RefCell<ControllerOverrideIvars>]
    struct ControllerOverride;

    unsafe impl NSObjectProtocol for ControllerOverride {}



















    impl ControllerOverride {
        #[unsafe(method(_requestContextWithRequests:error:))]

        unsafe fn request_context_with_requests_error(
            &self,
            requests: *mut AnyObject,
            error: *mut *mut NSError,
        ) -> *mut AnyObject {
            let context: *mut AnyObject = unsafe {
                msg_send![super(self), _requestContextWithRequests: requests, error: error]
            };
            if context.is_null() {
                return context;
            }
            let context_ref: &AnyObject = unsafe { &*context };

            let ivars = self.ivars().borrow();
            let is_create = ivars.is_create;
            let mut security_key = false;
            let mut options: *mut AnyObject = ptr::null_mut();

            unsafe {
                if is_create {
                    if responds_to(context_ref, sel!(platformKeyCredentialCreationOptions)) {
                        options = msg_send![context_ref, platformKeyCredentialCreationOptions];
                    }
                    if options.is_null()
                        && responds_to(context_ref, sel!(securityKeyCredentialCreationOptions))
                    {
                        options = msg_send![context_ref, securityKeyCredentialCreationOptions];
                        security_key = true;
                    }
                } else {
                    if responds_to(context_ref, sel!(platformKeyCredentialAssertionOptions)) {
                        options = msg_send![context_ref, platformKeyCredentialAssertionOptions];
                    }
                    if options.is_null()
                        && responds_to(context_ref, sel!(securityKeyCredentialAssertionOptions))
                    {
                        options = msg_send![context_ref, securityKeyCredentialAssertionOptions];
                    }
                }
            }

            if options.is_null() {
                return context;
            }
            let options_ref: &AnyObject = unsafe { &*options };

            if let Some(hash) = &ivars.client_data_hash {






                unsafe {
                    if responds_to(options_ref, sel!(setClientDataHash:)) {
                        let _: () = msg_send![options_ref, setClientDataHash: &**hash];
                    }
                    if responds_to(options_ref, sel!(setChallenge:)) {
                        let nil_data: *mut NSData = ptr::null_mut();
                        let _: () = msg_send![options_ref, setChallenge: nil_data];
                    }
                }
            }

            if is_create {
                if let Some(algs) = &ivars.supported_algorithms {



                    unsafe {
                        if responds_to(options_ref, sel!(setSupportedAlgorithmIdentifiers:)) {
                            let _: () = msg_send![
                                options_ref,
                                setSupportedAlgorithmIdentifiers: &**algs
                            ];
                        }
                    }
                }
                if !security_key {




                    unsafe {
                        if responds_to(options_ref, sel!(setShouldRequireResidentKey:)) {
                            let flag: bool = ivars.require_resident_key;
                            let _: () =
                                msg_send![options_ref, setShouldRequireResidentKey: flag];
                        }
                    }
                }
                if let Some(excluded) = &ivars.exclude_credentials {



                    unsafe {
                        if responds_to(options_ref, sel!(setExcludedCredentials:))
                            && excluded.count() > 0
                        {
                            let _: () =
                                msg_send![options_ref, setExcludedCredentials: &**excluded];
                        }
                    }
                }
            }

            context
        }
    }
);

impl ControllerOverride {
    fn new(
        requests: &NSArray<ASAuthorizationRequest>,
        ivars: ControllerOverrideIvars,
    ) -> Retained<Self> {
        let this = Self::alloc().set_ivars(core::cell::RefCell::new(ivars));
        unsafe { msg_send![super(this), initWithAuthorizationRequests: requests] }
    }
}

unsafe fn responds_to(obj: &AnyObject, sel: Sel) -> bool {
    unsafe { msg_send![obj, respondsToSelector: sel] }
}

fn controller_private_context_selector_available() -> bool {
    let cls = <ASAuthorizationController as ClassType>::class();
    unsafe { msg_send![cls, instancesRespondToSelector: sel!(_requestContextWithRequests:error:)] }
}

fn ns_string(value: &str) -> Retained<NSString> {
    NSString::from_str(value)
}

fn ns_data(bytes: &[u8]) -> Retained<NSData> {
    NSData::with_bytes(bytes)
}

fn copy_ns_string(value: &NSString) -> Retained<NSString> {
    value.copy()
}

fn user_verification_value(value: u32) -> Retained<NSString> {
    let opt = match value {
        1 => unsafe { ASAuthorizationPublicKeyCredentialUserVerificationPreferenceRequired },
        3 => unsafe { ASAuthorizationPublicKeyCredentialUserVerificationPreferenceDiscouraged },
        _ => unsafe { ASAuthorizationPublicKeyCredentialUserVerificationPreferencePreferred },
    };
    opt.map(copy_ns_string).unwrap_or_else(|| ns_string(""))
}

fn attestation_value(value: u32, security_key: bool) -> Retained<NSString> {
    let opt = if !security_key {
        unsafe { ASAuthorizationPublicKeyCredentialAttestationKindNone }
    } else if value == 3 {
        unsafe { ASAuthorizationPublicKeyCredentialAttestationKindDirect }
    } else if value == 2 {
        unsafe { ASAuthorizationPublicKeyCredentialAttestationKindIndirect }
    } else {
        unsafe { ASAuthorizationPublicKeyCredentialAttestationKindNone }
    };
    opt.map(copy_ns_string).unwrap_or_else(|| ns_string(""))
}

fn credential_parameters_array(
    algs: &[i32],
) -> Retained<NSArray<ASAuthorizationPublicKeyCredentialParameters>> {
    let mut params: Vec<Retained<ASAuthorizationPublicKeyCredentialParameters>> = algs
        .iter()
        .map(|alg| unsafe {
            ASAuthorizationPublicKeyCredentialParameters::initWithAlgorithm(
                ASAuthorizationPublicKeyCredentialParameters::alloc(),
                *alg as ASCOSEAlgorithmIdentifier,
            )
        })
        .collect();
    if params.is_empty() {
        params.push(unsafe {
            ASAuthorizationPublicKeyCredentialParameters::initWithAlgorithm(
                ASAuthorizationPublicKeyCredentialParameters::alloc(),
                objc2_authentication_services::ASCOSEAlgorithmIdentifierES256,
            )
        });
    }
    NSArray::from_retained_slice(&params)
}

fn supported_algorithm_numbers(algs: &[i32]) -> Retained<NSArray<NSNumber>> {
    let mut nums: Vec<Retained<NSNumber>> = algs
        .iter()
        .map(|a| NSNumber::new_isize(*a as isize))
        .collect();
    if nums.is_empty() {
        nums.push(NSNumber::new_isize(
            objc2_authentication_services::ASCOSEAlgorithmIdentifierES256,
        ));
    }
    NSArray::from_retained_slice(&nums)
}

fn security_key_transport_array(transports: u32) -> Retained<NSArray<NSString>> {
    let mut list: Vec<Retained<NSString>> = Vec::new();
    if transports & 0x0000_0001 != 0 {
        list.push(copy_ns_string(unsafe {
            ASAuthorizationSecurityKeyPublicKeyCredentialDescriptorTransportUSB
        }));
    }
    if transports & 0x0000_0002 != 0 {
        list.push(copy_ns_string(unsafe {
            ASAuthorizationSecurityKeyPublicKeyCredentialDescriptorTransportNFC
        }));
    }
    if transports & 0x0000_0004 != 0 {
        list.push(copy_ns_string(unsafe {
            ASAuthorizationSecurityKeyPublicKeyCredentialDescriptorTransportBluetooth
        }));
    }
    NSArray::from_retained_slice(&list)
}

fn platform_descriptor_array(
    descriptors: &[DescriptorInput],
) -> Retained<NSArray<ASAuthorizationPlatformPublicKeyCredentialDescriptor>> {
    let entries: Vec<Retained<ASAuthorizationPlatformPublicKeyCredentialDescriptor>> = descriptors
        .iter()
        .map(|d| {
            let data = ns_data(&d.id);
            unsafe {
                ASAuthorizationPlatformPublicKeyCredentialDescriptor::initWithCredentialID(
                    ASAuthorizationPlatformPublicKeyCredentialDescriptor::alloc(),
                    &data,
                )
            }
        })
        .collect();
    NSArray::from_retained_slice(&entries)
}

fn security_key_descriptor_array(
    descriptors: &[DescriptorInput],
) -> Retained<NSArray<ASAuthorizationSecurityKeyPublicKeyCredentialDescriptor>> {
    let entries: Vec<Retained<ASAuthorizationSecurityKeyPublicKeyCredentialDescriptor>> =
        descriptors
            .iter()
            .map(|d| {
                let data = ns_data(&d.id);
                let transports = security_key_transport_array(d.transports);
                unsafe {
                    ASAuthorizationSecurityKeyPublicKeyCredentialDescriptor::initWithCredentialID_transports(
                        ASAuthorizationSecurityKeyPublicKeyCredentialDescriptor::alloc(),
                        &data,
                        &transports,
                    )
                }
            })
            .collect();
    NSArray::from_retained_slice(&entries)
}

fn apply_registration_setup(
    request: &ProtocolObject<dyn ASAuthorizationPublicKeyCredentialRegistrationRequest>,
    user_verification: u32,
    attestation: u32,
    security_key: bool,
    credential_parameters: Option<&NSArray<ASAuthorizationPublicKeyCredentialParameters>>,
) {
    unsafe {
        let uv = user_verification_value(user_verification);
        request.setUserVerificationPreference(&uv);
        let att = attestation_value(attestation, security_key);
        request.setAttestationPreference(&att);
    }
    if let (true, Some(params)) = (security_key, credential_parameters) {
        unsafe {
            let proto_ptr: *const ProtocolObject<
                dyn ASAuthorizationPublicKeyCredentialRegistrationRequest,
            > = request;
            let sk_req = &*(proto_ptr
                as *const ASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest);
            sk_req.setCredentialParameters(params);
        }
    }
}

fn apply_assertion_setup(
    request: &ProtocolObject<dyn ASAuthorizationPublicKeyCredentialAssertionRequest>,
    user_verification: u32,
) {
    unsafe {
        let uv = user_verification_value(user_verification);
        request.setUserVerificationPreference(&uv);
    }
}

fn ns_string_to_string(s: &NSString) -> String {
    s.to_string()
}

fn format_ns_error(error: &NSError) -> String {
    let desc = error.localizedDescription();
    let mut message = ns_string_to_string(&desc);
    if message.is_empty() {
        message = "WebAuthn operation failed".to_owned();
    }
    let domain = error.domain();
    let domain_str = ns_string_to_string(&domain);
    let code = error.code();
    if !domain_str.is_empty() {
        message.push_str(&format!(" ({domain_str} {code})"));
    }
    if let Some(reason) = error.localizedFailureReason() {
        let reason = ns_string_to_string(&reason);
        if !reason.is_empty() {
            message.push_str("; reason: ");
            message.push_str(&reason);
        }
    }
    if let Some(suggestion) = error.localizedRecoverySuggestion() {
        let suggestion = ns_string_to_string(&suggestion);
        if !suggestion.is_empty() {
            message.push_str("; suggestion: ");
            message.push_str(&suggestion);
        }
    }

    let info = error.userInfo();
    let debug_key = NSString::from_str("NSDebugDescription");
    if let Some(value) = info.objectForKey(&debug_key) {
        let value_obj: &AnyObject = &value;
        let ns_string_cls = <NSString as ClassType>::class();

        let is_string: bool = unsafe { msg_send![value_obj, isKindOfClass: ns_string_cls] };
        if is_string {
            let s: &NSString = unsafe { &*(value_obj as *const _ as *const NSString) };
            let text = ns_string_to_string(s);
            if !text.is_empty() {
                message.push_str("; debug: ");
                message.push_str(&text);
            }
        }
    }
    let underlying_key = NSString::from_str("NSUnderlyingError");
    if let Some(value) = info.objectForKey(&underlying_key) {
        let value_obj: &AnyObject = &value;
        let ns_error_cls = <NSError as ClassType>::class();

        let is_error: bool = unsafe { msg_send![value_obj, isKindOfClass: ns_error_cls] };
        if is_error {
            let err: &NSError = unsafe { &*(value_obj as *const _ as *const NSError) };
            message.push_str("; underlying: ");
            message.push_str(&format_ns_error(err));
        }
    }
    message
}

fn extract_registration(credential: &AnyObject) -> CompletionPayload {
    unsafe {
        if !responds_to(credential, sel!(credentialID))
            || !responds_to(credential, sel!(rawAttestationObject))
        {
            return CompletionPayload::Error(
                "Unexpected WebAuthn registration credential".to_owned(),
            );
        }
    }
    let raw_id_data: Retained<NSData> = unsafe { msg_send![credential, credentialID] };
    let attestation_data: Option<Retained<NSData>> =
        unsafe { msg_send![credential, rawAttestationObject] };
    CompletionPayload::Create {
        raw_id: raw_id_data.to_vec(),
        attestation_object: attestation_data.map(|d| d.to_vec()).unwrap_or_default(),
        attachment: attachment_kind(credential),
    }
}

fn extract_assertion(credential: &AnyObject) -> CompletionPayload {
    unsafe {
        if !responds_to(credential, sel!(credentialID))
            || !responds_to(credential, sel!(signature))
            || !responds_to(credential, sel!(rawAuthenticatorData))
        {
            return CompletionPayload::Error("Unexpected WebAuthn assertion credential".to_owned());
        }
    }
    let raw_id_data: Retained<NSData> = unsafe { msg_send![credential, credentialID] };
    let auth_data: Retained<NSData> = unsafe { msg_send![credential, rawAuthenticatorData] };
    let signature_data: Retained<NSData> = unsafe { msg_send![credential, signature] };
    let user_handle = unsafe {
        if responds_to(credential, sel!(userID)) {
            let data: Retained<NSData> = msg_send![credential, userID];
            let v = data.to_vec();
            if v.is_empty() { None } else { Some(v) }
        } else {
            None
        }
    };
    CompletionPayload::Get {
        raw_id: raw_id_data.to_vec(),
        authenticator_data: auth_data.to_vec(),
        signature: signature_data.to_vec(),
        user_handle,
        attachment: attachment_kind(credential),
    }
}

fn attachment_kind(credential: &AnyObject) -> AttachmentKind {
    unsafe {
        if responds_to(credential, sel!(attachment)) {
            let val: ASAuthorizationPublicKeyCredentialAttachment =
                msg_send![credential, attachment];
            if val == ASAuthorizationPublicKeyCredentialAttachment::Platform {
                return AttachmentKind::Platform;
            }
        }
    }
    AttachmentKind::CrossPlatform
}

struct CeremonyInputsCreate {
    rp_id: String,
    user_name: String,
    display_name: String,
    challenge: Vec<u8>,
    user_id: Vec<u8>,
    client_data_hash: Vec<u8>,
    pub_key_algs: Vec<i32>,
    exclude_credentials: Vec<DescriptorInput>,
    authenticator_attachment: u32,
    user_verification: u32,
    attestation: u32,
    require_resident_key: bool,
}

struct CeremonyInputsGet {
    rp_id: String,
    challenge: Vec<u8>,
    client_data_hash: Vec<u8>,
    allow_credentials: Vec<DescriptorInput>,
    authenticator_attachment: u32,
    user_verification: u32,
}

fn run_create(input: &CreateInput) -> std::result::Result<CreateResult, String> {
    let snapshot = CeremonyInputsCreate {
        rp_id: input.rp_id.clone(),
        user_name: input.user_name.clone(),
        display_name: if input.user_display_name.is_empty() {
            input.user_name.clone()
        } else {
            input.user_display_name.clone()
        },
        challenge: input.challenge.clone(),
        user_id: input.user_id.clone(),
        client_data_hash: input.client_data_hash.clone(),
        pub_key_algs: input.pub_key_algs.clone(),
        exclude_credentials: input.exclude_credentials.clone(),
        authenticator_attachment: input.authenticator_attachment,
        user_verification: input.user_verification,
        attestation: input.attestation,
        require_resident_key: input.require_resident_key,
    };

    let semaphore = DispatchSemaphore::new(0);
    let (tx, rx) = channel::<CompletionPayload>();

    let (setup_tx, setup_rx) = channel::<std::result::Result<MainThreadHandles, String>>();
    let semaphore_for_closure = semaphore.clone();

    DispatchQueue::main().exec_async(move || {
        let mtm = unsafe { MainThreadMarker::new_unchecked() };
        let result =
            build_create_controller(mtm, snapshot, tx.clone(), semaphore_for_closure.clone());
        match result {
            Ok(handles) => {
                handles.perform(mtm);

                let _ = setup_tx.send(Ok(handles));
            }
            Err(msg) => {
                let _ = setup_tx.send(Err(msg));
            }
        }
    });

    let handles = match setup_rx.recv() {
        Ok(Ok(h)) => h,
        Ok(Err(e)) => return Err(e),
        Err(_) => return Err("main-queue setup dispatch failed".to_owned()),
    };

    let timeout = if input.timeout_ms == 0 {
        DispatchTime::FOREVER
    } else {
        DispatchTime::NOW.time((input.timeout_ms as i64) * 1_000_000)
    };
    let wait = semaphore.try_acquire(timeout);
    if matches!(wait, Err(WaitError::Timeout)) {
        handles.cancel();
        return Err("WebAuthn registration timed out".to_owned());
    }
    drop(handles);

    let payload = rx
        .recv()
        .map_err(|_| "WebAuthn registration completed without payload".to_owned())?;
    match payload {
        CompletionPayload::Create {
            raw_id,
            attestation_object,
            attachment,
        } => Ok(CreateResult {
            raw_id,
            attestation_object,
            client_data_json: input.client_data_json.clone(),
            used_transport: attachment.transport_bit(),
        }),
        CompletionPayload::Error(msg) => Err(msg),
        CompletionPayload::Get { .. } => {
            Err("internal: assertion payload returned from registration ceremony".to_owned())
        }
    }
}

fn run_get(input: &GetInput) -> std::result::Result<GetResult, String> {
    let snapshot = CeremonyInputsGet {
        rp_id: input.rp_id.clone(),
        challenge: input.challenge.clone(),
        client_data_hash: input.client_data_hash.clone(),
        allow_credentials: input.allow_credentials.clone(),
        authenticator_attachment: input.authenticator_attachment,
        user_verification: input.user_verification,
    };

    let semaphore = DispatchSemaphore::new(0);
    let (tx, rx) = channel::<CompletionPayload>();
    let (setup_tx, setup_rx) = channel::<std::result::Result<MainThreadHandles, String>>();
    let semaphore_for_closure = semaphore.clone();

    DispatchQueue::main().exec_async(move || {
        let mtm = unsafe { MainThreadMarker::new_unchecked() };
        let result = build_get_controller(mtm, snapshot, tx.clone(), semaphore_for_closure.clone());
        match result {
            Ok(handles) => {
                handles.perform(mtm);
                let _ = setup_tx.send(Ok(handles));
            }
            Err(msg) => {
                let _ = setup_tx.send(Err(msg));
            }
        }
    });

    let handles = match setup_rx.recv() {
        Ok(Ok(h)) => h,
        Ok(Err(e)) => return Err(e),
        Err(_) => return Err("main-queue setup dispatch failed".to_owned()),
    };

    let timeout = if input.timeout_ms == 0 {
        DispatchTime::FOREVER
    } else {
        DispatchTime::NOW.time((input.timeout_ms as i64) * 1_000_000)
    };
    let wait = semaphore.try_acquire(timeout);
    if matches!(wait, Err(WaitError::Timeout)) {
        handles.cancel();
        return Err("WebAuthn authentication timed out".to_owned());
    }
    drop(handles);

    let payload = rx
        .recv()
        .map_err(|_| "WebAuthn authentication completed without payload".to_owned())?;
    match payload {
        CompletionPayload::Get {
            raw_id,
            authenticator_data,
            signature,
            user_handle,
            attachment,
        } => Ok(GetResult {
            raw_id,
            authenticator_data,
            signature,
            user_handle,
            client_data_json: input.client_data_json.clone(),
            used_transport: attachment.transport_bit(),
        }),
        CompletionPayload::Error(msg) => Err(msg),
        CompletionPayload::Create { .. } => {
            Err("internal: registration payload returned from assertion ceremony".to_owned())
        }
    }
}

struct MainThreadHandlesInner {
    controller: Retained<ControllerOverride>,

    #[allow(dead_code)]
    delegate: Retained<WebAuthnDelegate>,
}

struct MainThreadHandles(MainThreadBound<MainThreadHandlesInner>);

impl MainThreadHandles {
    fn new(mtm: MainThreadMarker, inner: MainThreadHandlesInner) -> Self {
        Self(MainThreadBound::new(inner, mtm))
    }

    fn perform(&self, mtm: MainThreadMarker) {
        unsafe { self.0.get(mtm).controller.performRequests() };
    }

    fn cancel(self) {
        DispatchQueue::main().exec_async(move || {
            let this = self;

            let mtm = unsafe { MainThreadMarker::new_unchecked() };
            unsafe { this.0.get(mtm).controller.cancel() };

            let _ = this;
        });
    }
}

fn build_create_controller(
    mtm: MainThreadMarker,
    s: CeremonyInputsCreate,
    sender: Sender<CompletionPayload>,
    semaphore: DispatchRetained<DispatchSemaphore>,
) -> std::result::Result<MainThreadHandles, String> {
    if !controller_private_context_selector_available() {
        return Err(
            "AuthenticationServices request-context override is unavailable on this macOS version"
                .to_owned(),
        );
    }

    let rp_id_ns = ns_string(&s.rp_id);
    let user_name_ns = ns_string(&s.user_name);
    let display_name_ns = ns_string(&s.display_name);
    let challenge_ns = ns_data(&s.challenge);
    let user_id_ns = ns_data(&s.user_id);
    let credential_params = credential_parameters_array(&s.pub_key_algs);
    let supported_algorithms = supported_algorithm_numbers(&s.pub_key_algs);
    let platform_excluded = platform_descriptor_array(&s.exclude_credentials);
    let security_key_excluded = security_key_descriptor_array(&s.exclude_credentials);

    let requests: Retained<NSMutableArray<ASAuthorizationRequest>> = NSMutableArray::new();

    if s.authenticator_attachment == 0 || s.authenticator_attachment == 1 {
        let provider = unsafe {
            ASAuthorizationPlatformPublicKeyCredentialProvider::initWithRelyingPartyIdentifier(
                ASAuthorizationPlatformPublicKeyCredentialProvider::alloc(),
                &rp_id_ns,
            )
        };
        let request = unsafe {
            provider.createCredentialRegistrationRequestWithChallenge_name_userID(
                &challenge_ns,
                &user_name_ns,
                &user_id_ns,
            )
        };
        unsafe {
            request.setDisplayName(Some(&display_name_ns));
        }
        let proto = ProtocolObject::from_ref(&*request);
        apply_registration_setup(proto, s.user_verification, s.attestation, false, None);
        let req_obj: &ASAuthorizationRequest =
            unsafe { &*(&*request as *const _ as *const ASAuthorizationRequest) };
        requests.addObject(req_obj);
    }
    if s.authenticator_attachment == 0 || s.authenticator_attachment == 2 {
        let provider = unsafe {
            ASAuthorizationSecurityKeyPublicKeyCredentialProvider::initWithRelyingPartyIdentifier(
                ASAuthorizationSecurityKeyPublicKeyCredentialProvider::alloc(),
                &rp_id_ns,
            )
        };
        let request = unsafe {
            provider.createCredentialRegistrationRequestWithChallenge_displayName_name_userID(
                &challenge_ns,
                &display_name_ns,
                &user_name_ns,
                &user_id_ns,
            )
        };
        let proto = ProtocolObject::from_ref(&*request);
        apply_registration_setup(
            proto,
            s.user_verification,
            s.attestation,
            true,
            Some(&credential_params),
        );
        if security_key_excluded.count() > 0 {
            unsafe { request.setExcludedCredentials(&security_key_excluded) };
        }
        let req_obj: &ASAuthorizationRequest =
            unsafe { &*(&*request as *const _ as *const ASAuthorizationRequest) };
        requests.addObject(req_obj);
    }

    if requests.count() == 0 {
        return Err("No supported WebAuthn registration requests were created".to_owned());
    }

    let excluded_for_override: Retained<NSArray<AnyObject>> = if security_key_excluded.count() > 0 {
        unsafe { Retained::cast_unchecked(security_key_excluded) }
    } else {
        unsafe { Retained::cast_unchecked(platform_excluded) }
    };

    let ivars = ControllerOverrideIvars {
        client_data_hash: if s.client_data_hash.is_empty() {
            None
        } else {
            Some(ns_data(&s.client_data_hash))
        },
        supported_algorithms: Some(supported_algorithms),
        exclude_credentials: Some(excluded_for_override),
        require_resident_key: s.require_resident_key,
        is_create: true,
    };

    let controller = ControllerOverride::new(&requests, ivars);
    let delegate = WebAuthnDelegate::new(mtm, CeremonyKind::Create, sender, semaphore);

    unsafe {
        controller.setDelegate(Some(ProtocolObject::from_ref(&*delegate)));
        controller.setPresentationContextProvider(Some(ProtocolObject::from_ref(&*delegate)));
    }

    Ok(MainThreadHandles::new(
        mtm,
        MainThreadHandlesInner {
            controller,
            delegate,
        },
    ))
}

fn build_get_controller(
    mtm: MainThreadMarker,
    s: CeremonyInputsGet,
    sender: Sender<CompletionPayload>,
    semaphore: DispatchRetained<DispatchSemaphore>,
) -> std::result::Result<MainThreadHandles, String> {
    if !controller_private_context_selector_available() {
        return Err(
            "AuthenticationServices request-context override is unavailable on this macOS version"
                .to_owned(),
        );
    }

    let rp_id_ns = ns_string(&s.rp_id);
    let challenge_ns = ns_data(&s.challenge);
    let platform_allow = platform_descriptor_array(&s.allow_credentials);
    let security_key_allow = security_key_descriptor_array(&s.allow_credentials);

    let requests: Retained<NSMutableArray<ASAuthorizationRequest>> = NSMutableArray::new();

    if s.authenticator_attachment == 0 || s.authenticator_attachment == 1 {
        let provider = unsafe {
            ASAuthorizationPlatformPublicKeyCredentialProvider::initWithRelyingPartyIdentifier(
                ASAuthorizationPlatformPublicKeyCredentialProvider::alloc(),
                &rp_id_ns,
            )
        };
        let request =
            unsafe { provider.createCredentialAssertionRequestWithChallenge(&challenge_ns) };
        let proto = ProtocolObject::from_ref(&*request);
        apply_assertion_setup(proto, s.user_verification);
        if platform_allow.count() > 0 {
            unsafe { request.setAllowedCredentials(&platform_allow) };
        }
        let req_obj: &ASAuthorizationRequest =
            unsafe { &*(&*request as *const _ as *const ASAuthorizationRequest) };
        requests.addObject(req_obj);
    }
    if s.authenticator_attachment == 0 || s.authenticator_attachment == 2 {
        let provider = unsafe {
            ASAuthorizationSecurityKeyPublicKeyCredentialProvider::initWithRelyingPartyIdentifier(
                ASAuthorizationSecurityKeyPublicKeyCredentialProvider::alloc(),
                &rp_id_ns,
            )
        };
        let request =
            unsafe { provider.createCredentialAssertionRequestWithChallenge(&challenge_ns) };
        let proto = ProtocolObject::from_ref(&*request);
        apply_assertion_setup(proto, s.user_verification);
        if security_key_allow.count() > 0 {
            unsafe { request.setAllowedCredentials(&security_key_allow) };
        }
        let req_obj: &ASAuthorizationRequest =
            unsafe { &*(&*request as *const _ as *const ASAuthorizationRequest) };
        requests.addObject(req_obj);
    }

    if requests.count() == 0 {
        return Err("No supported WebAuthn authentication requests were created".to_owned());
    }

    let ivars = ControllerOverrideIvars {
        client_data_hash: if s.client_data_hash.is_empty() {
            None
        } else {
            Some(ns_data(&s.client_data_hash))
        },
        supported_algorithms: None,
        exclude_credentials: None,
        require_resident_key: false,
        is_create: false,
    };

    let controller = ControllerOverride::new(&requests, ivars);
    let delegate = WebAuthnDelegate::new(mtm, CeremonyKind::Get, sender, semaphore);

    unsafe {
        controller.setDelegate(Some(ProtocolObject::from_ref(&*delegate)));
        controller.setPresentationContextProvider(Some(ProtocolObject::from_ref(&*delegate)));
    }

    Ok(MainThreadHandles::new(
        mtm,
        MainThreadHandlesInner {
            controller,
            delegate,
        },
    ))
}
