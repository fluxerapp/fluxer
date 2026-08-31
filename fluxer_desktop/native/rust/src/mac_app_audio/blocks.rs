// SPDX-License-Identifier: AGPL-3.0-or-later

use std::ffi::{c_char, c_void};

pub const BLOCK_HAS_COPY_DISPOSE: i32 = 1 << 25;
pub const BLOCK_HAS_SIGNATURE: i32 = 1 << 30;

pub const NS_ERROR_BLOCK_SIGNATURE: &[u8] = b"v16@?0@8\0";
pub const CONTENT_ERROR_BLOCK_SIGNATURE: &[u8] = b"v24@?0@8@16\0";

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct BlockDescriptorSig {
    pub reserved: u64,
    pub size: u64,
    pub signature: *const c_char,
}

pub type NsErrorUserInvoke = unsafe extern "C" fn(ctx: *mut c_void, err: *mut c_void);
pub type NsErrorInvoke = unsafe extern "C" fn(block: *mut NSErrorBlock, err: *mut c_void);

#[repr(C)]
pub struct NSErrorBlock {
    pub isa: *mut c_void,
    pub flags: i32,
    pub reserved: i32,
    pub invoke: NsErrorInvoke,
    pub descriptor: *const BlockDescriptorSig,
    pub ctx: *mut c_void,
    pub user_invoke: NsErrorUserInvoke,
}

pub type ContentErrorUserInvoke =
    unsafe extern "C" fn(ctx: *mut c_void, content: *mut c_void, err: *mut c_void);
pub type ContentErrorInvoke =
    unsafe extern "C" fn(block: *mut ContentErrorBlock, content: *mut c_void, err: *mut c_void);

#[repr(C)]
pub struct ContentErrorBlock {
    pub isa: *mut c_void,
    pub flags: i32,
    pub reserved: i32,
    pub invoke: ContentErrorInvoke,
    pub descriptor: *const BlockDescriptorSig,
    pub ctx: *mut c_void,
    pub user_invoke: ContentErrorUserInvoke,
}

pub const NS_ERROR_BLOCK_DESCRIPTOR: BlockDescriptorSig = BlockDescriptorSig {
    reserved: 0,
    size: std::mem::size_of::<NSErrorBlock>() as u64,
    signature: NS_ERROR_BLOCK_SIGNATURE.as_ptr().cast(),
};

pub const CONTENT_ERROR_BLOCK_DESCRIPTOR: BlockDescriptorSig = BlockDescriptorSig {
    reserved: 0,
    size: std::mem::size_of::<ContentErrorBlock>() as u64,
    signature: CONTENT_ERROR_BLOCK_SIGNATURE.as_ptr().cast(),
};

#[cfg(test)]
mod tests {
    use super::*;

    unsafe extern "C" fn ns_error_trampoline(block: *mut NSErrorBlock, err: *mut c_void) {
        if let Some(block) = unsafe { block.as_mut() } {
            unsafe { (block.user_invoke)(block.ctx, err) };
        }
    }

    unsafe extern "C" fn content_error_trampoline(
        block: *mut ContentErrorBlock,
        content: *mut c_void,
        err: *mut c_void,
    ) {
        if let Some(block) = unsafe { block.as_mut() } {
            unsafe { (block.user_invoke)(block.ctx, content, err) };
        }
    }

    #[repr(C)]
    struct RoundTripCtx {
        seen_err: *mut c_void,
        hit_count: u32,
    }

    unsafe extern "C" fn round_trip_user_invoke(ctx: *mut c_void, err: *mut c_void) {
        let ctx = unsafe { &mut *(ctx.cast::<RoundTripCtx>()) };
        ctx.seen_err = err;
        ctx.hit_count += 1;
    }

    #[repr(C)]
    struct ContentErrorRoundTripCtx {
        seen_content: *mut c_void,
        seen_err: *mut c_void,
        hit_count: u32,
    }

    unsafe extern "C" fn content_error_user_invoke(
        ctx: *mut c_void,
        content: *mut c_void,
        err: *mut c_void,
    ) {
        let ctx = unsafe { &mut *(ctx.cast::<ContentErrorRoundTripCtx>()) };
        ctx.seen_content = content;
        ctx.seen_err = err;
        ctx.hit_count += 1;
    }

    #[test]
    fn block_layout_offsets_match_clang_abi_spec() {
        assert_eq!(0, std::mem::offset_of!(NSErrorBlock, isa));
        assert_eq!(8, std::mem::offset_of!(NSErrorBlock, flags));
        assert_eq!(12, std::mem::offset_of!(NSErrorBlock, reserved));
        assert_eq!(16, std::mem::offset_of!(NSErrorBlock, invoke));
        assert_eq!(24, std::mem::offset_of!(NSErrorBlock, descriptor));
        assert_eq!(32, std::mem::offset_of!(NSErrorBlock, ctx));
        assert_eq!(40, std::mem::offset_of!(NSErrorBlock, user_invoke));
    }

    #[test]
    fn block_descriptor_sig_offsets_match_clang_abi_spec() {
        assert_eq!(0, std::mem::offset_of!(BlockDescriptorSig, reserved));
        assert_eq!(8, std::mem::offset_of!(BlockDescriptorSig, size));
        assert_eq!(16, std::mem::offset_of!(BlockDescriptorSig, signature));
    }

    #[test]
    fn block_round_trip_via_direct_trampoline_call() {
        let mut ctx = RoundTripCtx {
            seen_err: std::ptr::null_mut(),
            hit_count: 0,
        };
        let mut block = NSErrorBlock {
            isa: std::ptr::null_mut(),
            flags: BLOCK_HAS_SIGNATURE,
            reserved: 0,
            invoke: ns_error_trampoline,
            descriptor: &NS_ERROR_BLOCK_DESCRIPTOR,
            ctx: (&mut ctx as *mut RoundTripCtx).cast(),
            user_invoke: round_trip_user_invoke,
        };
        let sentinel = 0xCAFE_F00D_usize as *mut c_void;

        unsafe { (block.invoke)(&mut block, sentinel) };

        assert_eq!(1, ctx.hit_count);
        assert_eq!(sentinel, ctx.seen_err);
    }

    #[test]
    fn content_error_block_layout_offsets_match_clang_abi_spec() {
        assert_eq!(0, std::mem::offset_of!(ContentErrorBlock, isa));
        assert_eq!(16, std::mem::offset_of!(ContentErrorBlock, invoke));
        assert_eq!(24, std::mem::offset_of!(ContentErrorBlock, descriptor));
        assert_eq!(32, std::mem::offset_of!(ContentErrorBlock, ctx));
        assert_eq!(40, std::mem::offset_of!(ContentErrorBlock, user_invoke));
    }

    #[test]
    fn content_error_block_round_trip_via_direct_trampoline_call() {
        let mut ctx = ContentErrorRoundTripCtx {
            seen_content: std::ptr::null_mut(),
            seen_err: std::ptr::null_mut(),
            hit_count: 0,
        };
        let mut block = ContentErrorBlock {
            isa: std::ptr::null_mut(),
            flags: BLOCK_HAS_SIGNATURE,
            reserved: 0,
            invoke: content_error_trampoline,
            descriptor: &CONTENT_ERROR_BLOCK_DESCRIPTOR,
            ctx: (&mut ctx as *mut ContentErrorRoundTripCtx).cast(),
            user_invoke: content_error_user_invoke,
        };
        let content_sentinel = 0xDEAD_BEEF_usize as *mut c_void;
        let err_sentinel = 0xCAFE_F00D_usize as *mut c_void;

        unsafe { (block.invoke)(&mut block, content_sentinel, err_sentinel) };

        assert_eq!(1, ctx.hit_count);
        assert_eq!(content_sentinel, ctx.seen_content);
        assert_eq!(err_sentinel, ctx.seen_err);
    }
}
