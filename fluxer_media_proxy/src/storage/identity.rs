// SPDX-License-Identifier: AGPL-3.0-or-later

use sha2::{Digest, Sha256};

const REMOTE_IDENTITY_DOMAIN: &[u8] = b"fluxer.media-proxy.source-object.remote";
const LOCAL_IDENTITY_DOMAIN: &[u8] = b"fluxer.media-proxy.source-object.local";

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SourceObjectIdentity {
    cache_identity: String,
    content_length: u64,
    content_type: String,
    etag: Option<String>,
}

pub(super) struct RemoteSourceObject<'a> {
    pub(super) bucket: &'a str,
    pub(super) key: &'a str,
    pub(super) content_length: u64,
    pub(super) content_type: &'a str,
    pub(super) etag: Option<&'a str>,
    pub(super) last_modified: Option<&'a str>,
}

pub(super) struct LocalSourceObject<'a> {
    pub(super) bucket: &'a str,
    pub(super) key: &'a str,
    pub(super) content_length: u64,
    pub(super) content_type: &'a str,
    pub(super) modified_nanos: i128,
    pub(super) inode: u64,
}

impl SourceObjectIdentity {
    pub fn cache_identity(&self) -> &str {
        &self.cache_identity
    }

    pub fn content_length(&self) -> u64 {
        self.content_length
    }

    pub fn content_type(&self) -> &str {
        &self.content_type
    }

    pub(super) fn etag(&self) -> Option<&str> {
        self.etag.as_deref()
    }
}

pub(super) fn remote_source_object_identity(
    object: RemoteSourceObject<'_>,
) -> SourceObjectIdentity {
    let mut digest = Sha256::new();
    update_identity_field(&mut digest, REMOTE_IDENTITY_DOMAIN);
    update_identity_field(&mut digest, object.bucket.as_bytes());
    update_identity_field(&mut digest, object.key.as_bytes());
    update_identity_field(&mut digest, &object.content_length.to_be_bytes());
    update_identity_field(&mut digest, object.content_type.as_bytes());
    update_optional_identity_field(&mut digest, object.etag);
    update_optional_identity_field(&mut digest, object.last_modified);
    SourceObjectIdentity {
        cache_identity: hex::encode(digest.finalize()),
        content_length: object.content_length,
        content_type: object.content_type.to_owned(),
        etag: object.etag.map(ToOwned::to_owned),
    }
}

pub(super) fn local_source_object_identity(object: LocalSourceObject<'_>) -> SourceObjectIdentity {
    let mut digest = Sha256::new();
    update_identity_field(&mut digest, LOCAL_IDENTITY_DOMAIN);
    update_identity_field(&mut digest, object.bucket.as_bytes());
    update_identity_field(&mut digest, object.key.as_bytes());
    update_identity_field(&mut digest, &object.content_length.to_be_bytes());
    update_identity_field(&mut digest, object.content_type.as_bytes());
    update_identity_field(&mut digest, &object.modified_nanos.to_be_bytes());
    update_identity_field(&mut digest, &object.inode.to_be_bytes());
    SourceObjectIdentity {
        cache_identity: hex::encode(digest.finalize()),
        content_length: object.content_length,
        content_type: object.content_type.to_owned(),
        etag: None,
    }
}

fn update_optional_identity_field(digest: &mut Sha256, value: Option<&str>) {
    match value {
        Some(value) => {
            digest.update([1]);
            update_identity_field(digest, value.as_bytes());
        }
        None => digest.update([0]),
    }
}

fn update_identity_field(digest: &mut Sha256, value: &[u8]) {
    let length = u64::try_from(value.len()).expect("source identity field length must fit u64");
    digest.update(length.to_be_bytes());
    digest.update(value);
}
