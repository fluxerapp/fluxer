// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::api::generated::types as generated_types;

use super::client::{AdminApiClient, ApiError, ApiResult};
use super::types::{BanAvatarResult, BanCheckResult, BulkBanResult};

impl AdminApiClient {
    pub async fn ban_email(&self, email: &str) -> ApiResult<()> {
        self.create_blocklist_entry(
            "email",
            generated_types::BanEmailRequest {
                email: generated_types::EmailType::from(email.to_owned()),
            }
            .into(),
        )
        .await
    }

    pub async fn unban_email(&self, email: &str) -> ApiResult<()> {
        self.delete_blocklist_entry("email", email, None).await
    }

    pub async fn check_email_ban(&self, email: &str) -> ApiResult<BanCheckResult> {
        self.check_blocklist_entry("email", email, None).await
    }

    pub async fn ban_ip(&self, ip: &str) -> ApiResult<()> {
        self.create_blocklist_entry(
            "ip",
            generated_types::BanIpRequest { ip: ip.to_owned() }.into(),
        )
        .await
    }

    pub async fn unban_ip(&self, ip: &str) -> ApiResult<()> {
        self.delete_blocklist_entry("ip", ip, None).await
    }

    pub async fn check_ip_ban(&self, ip: &str) -> ApiResult<BanCheckResult> {
        self.check_blocklist_entry("ip", ip, None).await
    }

    pub async fn add_suspicious_email_domain(&self, domain: &str) -> ApiResult<()> {
        self.create_blocklist_entry(
            SUSPICIOUS_EMAIL_DOMAIN_LIST,
            suspicious_email_domain_request(domain)?.into(),
        )
        .await
    }

    pub async fn remove_suspicious_email_domain(&self, domain: &str) -> ApiResult<()> {
        self.delete_blocklist_entry(SUSPICIOUS_EMAIL_DOMAIN_LIST, domain, None)
            .await
    }

    pub async fn check_suspicious_email_domain(&self, domain: &str) -> ApiResult<BanCheckResult> {
        self.check_blocklist_entry(SUSPICIOUS_EMAIL_DOMAIN_LIST, domain, None)
            .await
    }

    pub async fn ban_phrase(&self, phrase: &str) -> ApiResult<()> {
        self.create_blocklist_entry(
            "phrase",
            generated_types::BanPhraseRequest {
                phrase: phrase.to_owned(),
            }
            .into(),
        )
        .await
    }

    pub async fn unban_phrase(&self, phrase: &str) -> ApiResult<()> {
        self.delete_blocklist_entry("phrase", phrase, None).await
    }

    pub async fn check_phrase_ban(&self, phrase: &str) -> ApiResult<BanCheckResult> {
        self.check_blocklist_entry("phrase", phrase, None).await
    }

    pub async fn ban_url(&self, url: &str) -> ApiResult<()> {
        self.create_blocklist_entry(
            "url",
            generated_types::BanUrlRequest {
                category: None,
                notes: None,
                severity: None,
                source_url: None,
                url: url.to_owned(),
            }
            .into(),
        )
        .await
    }

    pub async fn unban_url(&self, url: &str) -> ApiResult<()> {
        self.delete_blocklist_entry("url", url, None).await
    }

    pub async fn check_url_ban(&self, url: &str) -> ApiResult<BanCheckResult> {
        self.check_blocklist_entry("url", url, None).await
    }

    pub async fn ban_url_domain(&self, domain: &str, match_subdomains: bool) -> ApiResult<()> {
        self.create_blocklist_entry(
            "url-domain",
            generated_types::BanUrlDomainRequest {
                category: None,
                domain: domain.to_owned(),
                match_subdomains: Some(match_subdomains),
                notes: None,
                severity: None,
                source_url: None,
            }
            .into(),
        )
        .await
    }

    pub async fn unban_url_domain(&self, domain: &str) -> ApiResult<()> {
        self.delete_blocklist_entry("url-domain", domain, None)
            .await
    }

    pub async fn check_url_domain_ban(&self, domain: &str) -> ApiResult<BanCheckResult> {
        self.check_blocklist_entry("url-domain", domain, None).await
    }

    pub async fn ban_file_sha(
        &self,
        sha256_hex: &str,
        audit_log_reason: Option<&str>,
    ) -> ApiResult<()> {
        let body = generated_types::AdminBlocklistEntryCreateRequest::from(
            generated_types::BanFileShaRequest {
                category: None,
                content_type: None,
                notes: None,
                severity: None,
                sha256_hex: sha256_hex.to_owned(),
                source_url: None,
            },
        );
        self.post_void_with_reason(
            "/admin/blocklists/file-sha/entries",
            Some(&serde_json::to_value(&body).map_err(|e| ApiError::Parse(e.to_string()))?),
            audit_log_reason,
        )
        .await
    }

    pub async fn unban_file_sha(
        &self,
        sha256_hex: &str,
        audit_log_reason: Option<&str>,
    ) -> ApiResult<()> {
        self.delete_void_with_reason(
            &blocklist_entry_path("file-sha", sha256_hex),
            None,
            audit_log_reason,
        )
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

    pub async fn ban_avatar_hash(&self, hash_short: &str) -> ApiResult<()> {
        self.create_blocklist_entry(
            "avatar-hash",
            generated_types::BanAvatarHashRequest {
                category: None,
                hashes: vec![hash_short.to_owned()],
                notes: None,
                reason: None,
                severity: None,
                source_url: None,
            }
            .into(),
        )
        .await
    }

    pub async fn unban_avatar_hash(&self, hash_short: &str) -> ApiResult<()> {
        self.delete_blocklist_entry("avatar-hash", hash_short, None)
            .await
    }

    pub async fn check_avatar_hash_ban(&self, hash_short: &str) -> ApiResult<BanCheckResult> {
        self.check_blocklist_entry("avatar-hash", hash_short, None)
            .await
    }

    pub async fn ban_user_avatar(&self, user_id: &str) -> ApiResult<BanAvatarResult> {
        let body = generated_types::BanUserAvatarRequest::default();
        let response = self
            .generated()
            .ban_admin_user_avatar(
                &generated_types::SnowflakeType::from(user_id.to_owned()),
                &body,
            )
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn ban_profile_substring(&self, scope: &str, substring: &str) -> ApiResult<()> {
        self.create_blocklist_entry(
            PROFILE_SUBSTRING_LIST,
            profile_substring_request(scope, substring)?.into(),
        )
        .await
    }

    pub async fn unban_profile_substring(&self, scope: &str, substring: &str) -> ApiResult<()> {
        self.delete_blocklist_entry(PROFILE_SUBSTRING_LIST, substring, Some(scope))
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
    ) -> ApiResult<()> {
        self.generated()
            .create_admin_blocklist_entry(list_type, &body)
            .await
            .map_err(|e| self.generated_error(e))?;
        Ok(())
    }

    async fn delete_blocklist_entry(
        &self,
        list_type: &str,
        entry_value: &str,
        scope: Option<&str>,
    ) -> ApiResult<()> {
        let scope = scope.map(blocklist_delete_scope).transpose()?;
        self.generated()
            .delete_admin_blocklist_entry(list_type, entry_value, scope)
            .await
            .map_err(|e| self.generated_error(e))?;
        Ok(())
    }

    async fn check_blocklist_entry(
        &self,
        list_type: &str,
        entry_value: &str,
        scope: Option<&str>,
    ) -> ApiResult<BanCheckResult> {
        let scope = scope.map(blocklist_get_scope).transpose()?;
        let response = self
            .generated()
            .get_admin_blocklist_entry(list_type, entry_value, scope)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }
}

const SUSPICIOUS_EMAIL_DOMAIN_LIST: &str = "email-domain-suspicious";

const PROFILE_SUBSTRING_LIST: &str = "profile-substring";

fn blocklist_entry_path(list_type: &str, entry_value: &str) -> String {
    format!(
        "/admin/blocklists/{}/entries/{}",
        urlencoding::encode(list_type),
        urlencoding::encode(entry_value)
    )
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

fn suspicious_email_domain_request(
    domain: &str,
) -> ApiResult<generated_types::SuspiciousEmailDomainRequest> {
    Ok(generated_types::SuspiciousEmailDomainRequest {
        domain: generated_types::SuspiciousEmailDomainRequestDomain::try_from(domain)
            .map_err(|e| ApiError::Parse(e.to_string()))?,
    })
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
