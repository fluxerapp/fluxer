// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::api::generated::types as generated_types;

use super::client::{AdminApiClient, ApiError, ApiResult};
use super::types::{BanCheckResult, BlocklistEntryPage, BulkBanResult};

impl AdminApiClient {
    pub async fn ban_email(&self, email: &str, audit_log_reason: Option<&str>) -> ApiResult<()> {
        self.create_blocklist_entry(
            "email",
            generated_types::AdminBlocklistEntryCreateRequest::from(
                generated_types::BanEmailRequest {
                    email: generated_types::EmailBlocklistEntryType::from(email.to_owned()),
                },
            ),
            audit_log_reason,
        )
        .await
    }

    pub async fn unban_email(&self, email: &str, audit_log_reason: Option<&str>) -> ApiResult<()> {
        self.delete_blocklist_entry("email", email, None, audit_log_reason)
            .await
    }

    pub async fn check_email_ban(&self, email: &str) -> ApiResult<BanCheckResult> {
        self.check_blocklist_entry("email", email, None).await
    }

    pub async fn ban_ip(
        &self,
        ip: &str,
        duration_hours: u32,
        audit_log_reason: Option<&str>,
    ) -> ApiResult<()> {
        self.create_blocklist_entry(
            "ip",
            generated_types::AdminBlocklistEntryCreateRequest::from(
                generated_types::BanIpRequest {
                    duration_hours: Some(
                        i32::try_from(duration_hours)
                            .map_err(|e| ApiError::Parse(e.to_string()))?
                            .into(),
                    ),
                    ip: ip.to_owned(),
                },
            ),
            audit_log_reason,
        )
        .await
    }

    pub async fn unban_ip(&self, ip: &str, audit_log_reason: Option<&str>) -> ApiResult<()> {
        self.delete_blocklist_entry("ip", ip, None, audit_log_reason)
            .await
    }

    pub async fn check_ip_ban(&self, ip: &str) -> ApiResult<BanCheckResult> {
        self.check_blocklist_entry("ip", ip, None).await
    }

    pub async fn ban_phrase(&self, phrase: &str, audit_log_reason: Option<&str>) -> ApiResult<()> {
        self.create_blocklist_entry(
            "phrase",
            generated_types::AdminBlocklistEntryCreateRequest::from(
                generated_types::BanPhraseRequest {
                    phrase: phrase.to_owned(),
                },
            ),
            audit_log_reason,
        )
        .await
    }

    pub async fn unban_phrase(
        &self,
        phrase: &str,
        audit_log_reason: Option<&str>,
    ) -> ApiResult<()> {
        self.delete_blocklist_entry("phrase", phrase, None, audit_log_reason)
            .await
    }

    pub async fn check_phrase_ban(&self, phrase: &str) -> ApiResult<BanCheckResult> {
        self.check_blocklist_entry("phrase", phrase, None).await
    }

    pub async fn ban_url(&self, url: &str, audit_log_reason: Option<&str>) -> ApiResult<()> {
        self.create_blocklist_entry(
            "url",
            generated_types::AdminBlocklistEntryCreateRequest::from(
                generated_types::BanUrlRequest {
                    category: None,
                    notes: None,
                    severity: None,
                    source_url: None,
                    url: url.to_owned(),
                },
            ),
            audit_log_reason,
        )
        .await
    }

    pub async fn unban_url(&self, url: &str, audit_log_reason: Option<&str>) -> ApiResult<()> {
        self.delete_blocklist_entry("url", url, None, audit_log_reason)
            .await
    }

    pub async fn check_url_ban(&self, url: &str) -> ApiResult<BanCheckResult> {
        self.check_blocklist_entry("url", url, None).await
    }

    pub async fn ban_url_domain(
        &self,
        domain: &str,
        match_subdomains: bool,
        audit_log_reason: Option<&str>,
    ) -> ApiResult<()> {
        self.create_blocklist_entry(
            "url-domain",
            generated_types::AdminBlocklistEntryCreateRequest::from(
                generated_types::BanUrlDomainRequest {
                    category: None,
                    domain: domain.to_owned(),
                    match_subdomains,
                    notes: None,
                    severity: None,
                    source_url: None,
                },
            ),
            audit_log_reason,
        )
        .await
    }

    pub async fn unban_url_domain(
        &self,
        domain: &str,
        audit_log_reason: Option<&str>,
    ) -> ApiResult<()> {
        self.delete_blocklist_entry("url-domain", domain, None, audit_log_reason)
            .await
    }

    pub async fn check_url_domain_ban(&self, domain: &str) -> ApiResult<BanCheckResult> {
        self.check_blocklist_entry("url-domain", domain, None).await
    }

    pub async fn list_url_domain_entries(
        &self,
        after: Option<&str>,
    ) -> ApiResult<BlocklistEntryPage> {
        let list_type = blocklist_list_type("url-domain")?;
        let response = self
            .generated()
            .list_admin_blocklist_entries(list_type, after, Some(BLOCKLIST_PAGE_SIZE), None)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn ban_file_sha(
        &self,
        sha256_hex: &str,
        audit_log_reason: Option<&str>,
    ) -> ApiResult<()> {
        self.create_blocklist_entry(
            "file-sha",
            generated_types::AdminBlocklistEntryCreateRequest::from(
                generated_types::BanFileShaRequest {
                    category: None,
                    content_type: None,
                    notes: None,
                    severity: None,
                    sha256_hex: sha256_hex.to_owned(),
                    source_url: None,
                },
            ),
            audit_log_reason,
        )
        .await
    }

    pub async fn unban_file_sha(
        &self,
        sha256_hex: &str,
        audit_log_reason: Option<&str>,
    ) -> ApiResult<()> {
        self.delete_blocklist_entry("file-sha", sha256_hex, None, audit_log_reason)
            .await
    }

    pub async fn check_file_sha_ban(&self, sha256_hex: &str) -> ApiResult<BanCheckResult> {
        self.check_blocklist_entry("file-sha", sha256_hex, None)
            .await
    }

    pub async fn bulk_ban_file_shas(
        &self,
        sha256_list: &[String],
        audit_log_reason: Option<&str>,
    ) -> ApiResult<BulkBanResult> {
        let body = generated_types::BulkBanFileShasRequest {
            sha256_list: sha256_list.to_vec(),
        };
        self.put_typed_with_reason(
            "/admin/blocklists/file-sha/entries",
            &body,
            audit_log_reason,
        )
        .await
    }

    pub async fn ban_avatar_hash(
        &self,
        hash_short: &str,
        audit_log_reason: Option<&str>,
    ) -> ApiResult<()> {
        self.create_blocklist_entry(
            "avatar-hash",
            generated_types::AdminBlocklistEntryCreateRequest::from(
                generated_types::BanAvatarHashRequest {
                    category: None,
                    hashes: vec![hash_short.to_owned()],
                    notes: None,
                    reason: None,
                    severity: None,
                    source_url: None,
                },
            ),
            audit_log_reason,
        )
        .await
    }

    pub async fn unban_avatar_hash(
        &self,
        hash_short: &str,
        audit_log_reason: Option<&str>,
    ) -> ApiResult<()> {
        self.delete_blocklist_entry("avatar-hash", hash_short, None, audit_log_reason)
            .await
    }

    pub async fn check_avatar_hash_ban(&self, hash_short: &str) -> ApiResult<BanCheckResult> {
        self.check_blocklist_entry("avatar-hash", hash_short, None)
            .await
    }

    pub async fn ban_profile_substring(
        &self,
        scope: &str,
        substring: &str,
        audit_log_reason: Option<&str>,
    ) -> ApiResult<()> {
        self.create_blocklist_entry(
            PROFILE_SUBSTRING_LIST,
            generated_types::AdminBlocklistEntryCreateRequest::from(profile_substring_request(
                scope, substring,
            )?),
            audit_log_reason,
        )
        .await
    }

    pub async fn unban_profile_substring(
        &self,
        scope: &str,
        substring: &str,
        audit_log_reason: Option<&str>,
    ) -> ApiResult<()> {
        self.delete_blocklist_entry(
            PROFILE_SUBSTRING_LIST,
            substring,
            Some(scope),
            audit_log_reason,
        )
        .await
    }

    pub async fn check_profile_substring_ban(
        &self,
        scope: &str,
        substring: &str,
    ) -> ApiResult<BanCheckResult> {
        self.check_blocklist_entry(PROFILE_SUBSTRING_LIST, substring, Some(scope))
            .await
    }

    async fn create_blocklist_entry(
        &self,
        list_type: &str,
        body: generated_types::AdminBlocklistEntryCreateRequest,
        audit_log_reason: Option<&str>,
    ) -> ApiResult<()> {
        let list_type = blocklist_list_type(list_type)?;
        self.generated_with_reason(audit_log_reason)?
            .create_admin_blocklist_entry(list_type, &body)
            .await
            .map(drop)
            .map_err(|error| self.generated_error(error))
    }

    async fn delete_blocklist_entry(
        &self,
        list_type: &str,
        entry_value: &str,
        scope: Option<&str>,
        audit_log_reason: Option<&str>,
    ) -> ApiResult<()> {
        let list_type = blocklist_list_type(list_type)?;
        let scope = scope.map(blocklist_delete_scope).transpose()?;
        self.generated_with_reason(audit_log_reason)?
            .delete_admin_blocklist_entry(list_type, entry_value, scope)
            .await
            .map(drop)
            .map_err(|error| self.generated_error(error))
    }

    async fn check_blocklist_entry(
        &self,
        list_type: &str,
        entry_value: &str,
        scope: Option<&str>,
    ) -> ApiResult<BanCheckResult> {
        let list_type = blocklist_list_type(list_type)?;
        let scope = scope.map(blocklist_get_scope).transpose()?;
        let response = self
            .generated()
            .get_admin_blocklist_entry(list_type, entry_value, scope)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }
}

const PROFILE_SUBSTRING_LIST: &str = "profile-substring";

const BLOCKLIST_PAGE_SIZE: &str = "200";

fn blocklist_list_type(list_type: &str) -> ApiResult<generated_types::AdminBlocklistListType> {
    generated_types::AdminBlocklistListType::try_from(list_type)
        .map_err(|e| ApiError::Parse(e.to_string()))
}

fn blocklist_get_scope(scope: &str) -> ApiResult<generated_types::GetAdminBlocklistEntryScope> {
    generated_types::GetAdminBlocklistEntryScope::try_from(scope)
        .map_err(|e| ApiError::Parse(e.to_string()))
}

fn blocklist_delete_scope(
    scope: &str,
) -> ApiResult<generated_types::DeleteAdminBlocklistEntryScope> {
    generated_types::DeleteAdminBlocklistEntryScope::try_from(scope)
        .map_err(|e| ApiError::Parse(e.to_string()))
}

fn profile_substring_request(
    scope: &str,
    substring: &str,
) -> ApiResult<generated_types::BanProfileSubstringRequest> {
    Ok(generated_types::BanProfileSubstringRequest {
        notes: None,
        reason: None,
        scope: generated_types::BanProfileSubstringRequestScope::try_from(scope)
            .map_err(|e| ApiError::Parse(e.to_string()))?,
        substrings: vec![substring.to_owned()],
    })
}
