// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::utils::plural::count_noun;
use crate::{
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{alert::alert_info, form::csrf_input, page_container::page_header},
        layout::admin_layout,
    },
};
use maud::{Markup, html};

pub struct BanConfig {
    pub title: &'static str,
    pub route: &'static str,
    pub input_label: &'static str,
    pub input_name: &'static str,
    pub input_type: &'static str,
    pub placeholder: &'static str,
    pub entity_name: &'static str,
    pub active_page: &'static str,
    pub show_bulk_tools: bool,
    pub show_duration: bool,
}

const IP_BAN_DURATIONS: &[(u32, &str)] = &[
    (24, "1 day"),
    (168, "7 days"),
    (720, "30 days"),
    (0, "Permanent"),
];

pub fn ip_ban_duration_label(hours: u32) -> String {
    IP_BAN_DURATIONS
        .iter()
        .find(|(value, _)| *value == hours)
        .map_or_else(
            || count_noun(u64::from(hours), "hour", "hours"),
            |(_, label)| (*label).to_owned(),
        )
}

pub const BAN_CONFIGS: &[BanConfig] = &[
    BanConfig {
        title: "IP Bans",
        route: "/ip-bans",
        input_label: "IP Address or CIDR",
        input_name: "ip",
        input_type: "text",
        placeholder: "192.168.1.1 or 192.168.0.0/16",
        entity_name: "IP/CIDR",
        active_page: "ip-bans",
        show_bulk_tools: false,
        show_duration: true,
    },
    BanConfig {
        title: "Email Bans",
        route: "/email-bans",
        input_label: "Email Address or Domain",
        input_name: "email",
        input_type: "text",
        placeholder: "user@example.com or @example.com",
        entity_name: "Email",
        active_page: "email-bans",
        show_bulk_tools: false,
        show_duration: false,
    },
    BanConfig {
        title: "Phrase Bans",
        route: "/phrase-bans",
        input_label: "Phrase",
        input_name: "phrase",
        input_type: "text",
        placeholder: "any substring to ban",
        entity_name: "Phrase",
        active_page: "phrase-bans",
        show_bulk_tools: false,
        show_duration: false,
    },
    BanConfig {
        title: "URL Blocklist",
        route: "/url-bans",
        input_label: "URL",
        input_name: "url",
        input_type: "text",
        placeholder: "https://example.com/malicious-page",
        entity_name: "URL",
        active_page: "url-bans",
        show_bulk_tools: false,
        show_duration: false,
    },
    BanConfig {
        title: "File SHA Blocklist",
        route: "/file-sha-bans",
        input_label: "SHA-256 Hash",
        input_name: "sha256_hex",
        input_type: "text",
        placeholder: "64-character hex SHA-256 hash",
        entity_name: "SHA-256",
        active_page: "file-sha-bans",
        show_bulk_tools: true,
        show_duration: false,
    },
    BanConfig {
        title: "Avatar Hash Blocklist",
        route: "/avatar-hash-bans",
        input_label: "Avatar Hash",
        input_name: "hash_short",
        input_type: "text",
        placeholder: "8-char MD5 prefix (e.g. a1b2c3d4)",
        entity_name: "Avatar Hash",
        active_page: "avatar-hash-bans",
        show_bulk_tools: false,
        show_duration: false,
    },
    BanConfig {
        title: "URL Domain Blocklist",
        route: "/url-domain-bans",
        input_label: "URL Domain",
        input_name: "domain",
        input_type: "text",
        placeholder: "example.com",
        entity_name: "Domain",
        active_page: "url-domain-bans",
        show_bulk_tools: false,
        show_duration: false,
    },
    BanConfig {
        title: "Profile Substring Blocklist",
        route: "/profile-substring-bans",
        input_label: "Substring",
        input_name: "substring",
        input_type: "text",
        placeholder: "offensive substring",
        entity_name: "Substring",
        active_page: "profile-substring-bans",
        show_bulk_tools: false,
        show_duration: false,
    },
];

pub fn get_ban_config(active_page: &str) -> Option<&'static BanConfig> {
    BAN_CONFIGS.iter().find(|c| c.active_page == active_page)
}

pub fn bans_page(
    config: &AdminConfig,
    auth: &AuthContext,
    ban_cfg: &BanConfig,
    flash: Option<&crate::api::types::FlashMessage>,
    csrf_token: &str,
    username_sign_in: bool,
) -> Markup {
    let base = &config.base_path;
    let content = html! {
        (page_header(ban_cfg.title, None))
        @if username_sign_in && ban_cfg.active_page == "email-bans" {
            div class="mb-6" {
                (alert_info(html! { "People sign in with a username on this instance. Accounts have no email address, so email bans have no effect." }))
            }
        }
        div class="grid gap-6 lg:grid-cols-2" {
            (ban_card(base, ban_cfg, csrf_token))
            (check_ban_card(base, ban_cfg, csrf_token))
        }
        @if ban_cfg.show_bulk_tools {
            div class="mt-6 grid gap-6 lg:grid-cols-2" {
                (bulk_ban_card(base, ban_cfg, csrf_token))
                (file_upload_ban_card(base, ban_cfg, csrf_token))
            }
        }
        div class="mt-6" {
            (unban_card(base, ban_cfg, csrf_token))
        }
    };
    admin_layout(
        config,
        auth,
        ban_cfg.title,
        ban_cfg.active_page,
        flash,
        content,
    )
}
fn ban_card(base: &str, cfg: &BanConfig, csrf_token: &str) -> Markup {
    html! {
        div class="rounded-lg border border-neutral-200 bg-white p-4 shadow-sm sm:p-6" {
            h3 class="text-base font-medium text-neutral-900 mb-4" {
                "Ban " (cfg.input_label)
            }
            form method="post" action={(base) (cfg.route) "?action=ban&_csrf=" (csrf_token)}
                hx-post={(base) (cfg.route) "?action=ban&_csrf=" (csrf_token)}
                hx-target="#flash-container"
                hx-swap="innerHTML" {
                (csrf_input(csrf_token))
                div class="space-y-4" {
                    (form_field(cfg.input_name, cfg.input_label, cfg.input_type, cfg.placeholder, true))
                    @if cfg.show_duration {
                        (duration_field())
                    }
                    (form_field("audit_log_reason", "Private reason (audit log, optional)", "text", "Why is this ban being applied?", false))
                    (submit_btn("Ban", cfg.entity_name, false))
                }
            }
        }
    }
}
fn check_ban_card(base: &str, cfg: &BanConfig, csrf_token: &str) -> Markup {
    html! {
        div class="rounded-lg border border-neutral-200 bg-white p-4 shadow-sm sm:p-6" {
            h3 class="text-base font-medium text-neutral-900 mb-4" {
                "Check " (cfg.input_label) " Ban Status"
            }
            form method="post" action={(base) (cfg.route) "?action=check&_csrf=" (csrf_token)}
                hx-post={(base) (cfg.route) "?action=check&_csrf=" (csrf_token)}
                hx-target="#flash-container"
                hx-swap="innerHTML" {
                (csrf_input(csrf_token))
                div class="space-y-4" {
                    (form_field(cfg.input_name, cfg.input_label, cfg.input_type, cfg.placeholder, true))
                    (submit_btn("Check Status", "", false))
                }
            }
        }
    }
}
fn unban_card(base: &str, cfg: &BanConfig, csrf_token: &str) -> Markup {
    html! {
        div class="rounded-lg border border-neutral-200 bg-white p-4 shadow-sm sm:p-6" {
            h3 class="text-base font-medium text-neutral-900 mb-4" {
                "Remove " (cfg.input_label) " Ban"
            }
            form method="post" action={(base) (cfg.route) "?action=unban&_csrf=" (csrf_token)}
                hx-post={(base) (cfg.route) "?action=unban&_csrf=" (csrf_token)}
                hx-target="#flash-container"
                hx-swap="innerHTML" {
                (csrf_input(csrf_token))
                div class="space-y-4" {
                    (form_field(cfg.input_name, cfg.input_label, cfg.input_type, cfg.placeholder, true))
                    (form_field("audit_log_reason", "Private reason (audit log, optional)", "text", "Why is this ban being removed?", false))
                    (submit_btn("Unban", cfg.entity_name, true))
                }
            }
        }
    }
}
fn bulk_ban_card(base: &str, cfg: &BanConfig, csrf_token: &str) -> Markup {
    html! {
        div class="rounded-lg border border-neutral-200 bg-white p-4 shadow-sm sm:p-6" {
            h3 class="text-base font-medium text-neutral-900 mb-4" {
                "Bulk Ban " (cfg.entity_name)
            }
            p class="text-sm text-neutral-500 mb-4" {
                "Paste one " (cfg.entity_name) " per line. Invalid entries will be skipped."
            }
            form method="post" action={(base) (cfg.route) "?action=bulk-ban&_csrf=" (csrf_token)}
                hx-post={(base) (cfg.route) "?action=bulk-ban&_csrf=" (csrf_token)}
                hx-target="#flash-container"
                hx-swap="innerHTML" {
                (csrf_input(csrf_token))
                div class="space-y-4" {
                    (textarea_field("hashes", &format!("{} values (one per line)", cfg.entity_name), true))
                    (form_field("audit_log_reason", "Private reason (audit log, optional)", "text", "Why are these bans being applied?", false))
                    (submit_btn("Ban All", "", false))
                }
            }
        }
    }
}

fn file_upload_ban_card(base: &str, cfg: &BanConfig, csrf_token: &str) -> Markup {
    html! {
        div class="rounded-lg border border-neutral-200 bg-white p-4 shadow-sm sm:p-6" {
            h3 class="text-base font-medium text-neutral-900 mb-4" {
                "Upload files to hash and ban"
            }
            p class="text-sm text-neutral-500 mb-4" {
                "Upload image or video files. The browser computes SHA-256 for each file and submits the hashes."
            }
            div id="file-upload-zone"
                class="rounded-lg border-2 border-neutral-300 border-dashed p-6 text-center transition-colors hover:border-neutral-400" {
                input type="file" id="file-upload-input" multiple accept="image/*,video/*" class="hidden";
                div class="space-y-2" {
                    p class="text-neutral-600 text-sm" {
                        "Drag and drop files here, or "
                        button type="button" id="file-upload-browse"
                            class="font-medium text-blue-600 hover:text-blue-700" { "browse" }
                    }
                    p class="text-neutral-400 text-xs" {
                        "Supports images (PNG, JPEG, WebP, GIF) and videos (MP4, WebM)"
                    }
                }
            }
            div id="file-upload-status" class="hidden mt-4" {
                div class="mb-2 flex items-center justify-between" {
                    p class="text-sm text-neutral-700" {
                        span id="file-upload-progress" { "0/0" }
                        " files processed"
                    }
                    span id="file-upload-spinner" class="hidden text-neutral-400 text-xs" { "Processing..." }
                }
                div id="file-upload-list" class="max-h-60 space-y-1 overflow-y-auto" {}
            }
            form id="file-upload-ban-form" method="post"
                action={(base) (cfg.route) "?action=bulk-ban-files&_csrf=" (csrf_token)}
                class="hidden mt-4" {
                (csrf_input(csrf_token))
                input type="hidden" name="sha256_list" id="file-upload-sha256-list" value="";
                div class="space-y-4" {
                    (form_field("audit_log_reason", "Private reason (audit log, optional)", "text", "Why are these bans being applied?", false))
                    (submit_btn("Ban All Computed Hashes", "", false))
                }
            }
        }
        script defer {
            (maud::PreEscaped(r#"
(function() {
	var zone = document.getElementById('file-upload-zone');
	var input = document.getElementById('file-upload-input');
	var browse = document.getElementById('file-upload-browse');
	var statusDiv = document.getElementById('file-upload-status');
	var progressSpan = document.getElementById('file-upload-progress');
	var spinnerSpan = document.getElementById('file-upload-spinner');
	var listDiv = document.getElementById('file-upload-list');
	var banForm = document.getElementById('file-upload-ban-form');
	var sha256Input = document.getElementById('file-upload-sha256-list');
	if (!zone || !input || !browse || !statusDiv || !progressSpan || !spinnerSpan || !listDiv || !banForm || !sha256Input || !crypto.subtle) return;
	var sha256Results = [];
	var totalFiles = 0;
	var processedFiles = 0;
	browse.addEventListener('click', function() { input.click(); });
	zone.addEventListener('dragover', function(e) {
		e.preventDefault();
		zone.classList.add('border-blue-400', 'bg-blue-50');
	});
	zone.addEventListener('dragleave', function(e) {
		e.preventDefault();
		zone.classList.remove('border-blue-400', 'bg-blue-50');
	});
	zone.addEventListener('drop', function(e) {
		e.preventDefault();
		zone.classList.remove('border-blue-400', 'bg-blue-50');
		if (e.dataTransfer && e.dataTransfer.files.length > 0) processFiles(e.dataTransfer.files);
	});
	input.addEventListener('change', function() {
		if (input.files && input.files.length > 0) processFiles(input.files);
	});
	function processFiles(files) {
		sha256Results = [];
		processedFiles = 0;
		totalFiles = files.length;
		statusDiv.classList.remove('hidden');
		spinnerSpan.classList.remove('hidden');
		banForm.classList.add('hidden');
		listDiv.innerHTML = '';
		progressSpan.textContent = '0/' + totalFiles;
		for (var i = 0; i < files.length; i++) processFile(files[i], i);
	}
	function processFile(file, index) {
		var row = document.createElement('div');
		row.className = 'flex items-center gap-2 rounded px-2 py-1 text-sm';
		var statusIcon = document.createElement('span');
		statusIcon.className = 'flex-shrink-0';
		statusIcon.textContent = '...';
		statusIcon.id = 'file-icon-' + index;
		var nameSpan = document.createElement('span');
		nameSpan.className = 'min-w-0 flex-1 truncate text-xs';
		nameSpan.textContent = file.name;
		var detailSpan = document.createElement('span');
		detailSpan.className = 'flex-shrink-0 text-neutral-400 text-xs';
		detailSpan.id = 'file-detail-' + index;
		detailSpan.textContent = 'Computing SHA-256...';
		row.appendChild(statusIcon);
		row.appendChild(nameSpan);
		row.appendChild(detailSpan);
		listDiv.appendChild(row);
		var reader = new FileReader();
		reader.onload = function() {
			crypto.subtle.digest('SHA-256', reader.result).then(function(hashBuffer) {
				var hashArray = Array.from(new Uint8Array(hashBuffer));
				var sha256Hex = hashArray.map(function(b) { return b.toString(16).padStart(2, '0'); }).join('');
				sha256Results.push(sha256Hex);
				document.getElementById('file-detail-' + index).textContent = 'SHA: ' + sha256Hex.substring(0, 16) + '...';
				document.getElementById('file-icon-' + index).textContent = 'OK';
				fileComplete();
			}).catch(function() {
				document.getElementById('file-detail-' + index).textContent = 'Error computing SHA-256';
				document.getElementById('file-icon-' + index).textContent = 'ERR';
				fileComplete();
			});
		};
		reader.readAsArrayBuffer(file);
	}
	function fileComplete() {
		processedFiles++;
		progressSpan.textContent = processedFiles + '/' + totalFiles;
		if (processedFiles >= totalFiles) {
			spinnerSpan.classList.add('hidden');
			if (sha256Results.length > 0) {
				sha256Input.value = sha256Results.join('\n');
				banForm.classList.remove('hidden');
			}
		}
	}
})();
"#))
        }
    }
}

fn form_field(
    name: &str,
    label: &str,
    input_type: &str,
    placeholder: &str,
    required: bool,
) -> Markup {
    html! {
        div class="space-y-1" {
            label for=(name) class="block text-sm font-medium text-neutral-700" {
                (label)
            }
            input type=(input_type) id=(name) name=(name) placeholder=(placeholder)
                required[required]
                class="block w-full rounded-md border border-neutral-300 px-3 py-2 text-sm \
                       shadow-sm focus:border-brand-primary focus:outline-none focus:ring-1 \
                       focus:ring-brand-primary";
        }
    }
}

fn duration_field() -> Markup {
    html! {
        div class="space-y-1" {
            label for="duration_hours" class="block text-sm font-medium text-neutral-700" {
                "Duration"
            }
            select id="duration_hours" name="duration_hours"
                class="block w-full rounded-md border border-neutral-300 px-3 py-2 text-sm \
                       shadow-sm focus:border-brand-primary focus:outline-none focus:ring-1 \
                       focus:ring-brand-primary" {
                @for &(value, label) in IP_BAN_DURATIONS {
                    option value=(value) { (label) }
                }
            }
        }
    }
}

fn textarea_field(name: &str, label: &str, required: bool) -> Markup {
    html! {
        div class="space-y-1" {
            label for=(name) class="block text-sm font-medium text-neutral-700" {
                (label)
            }
            textarea id=(name) name=(name) rows="8" required[required]
                class="block w-full rounded-md border border-neutral-300 px-3 py-2 text-sm \
                       shadow-sm focus:border-brand-primary focus:outline-none focus:ring-1 \
                       focus:ring-brand-primary" {}
        }
    }
}

fn submit_btn(action: &str, entity: &str, danger: bool) -> Markup {
    let classes = if danger {
        "inline-flex items-center rounded-md bg-red-600 px-4 py-2 text-sm font-medium \
         text-white shadow-sm hover:bg-red-700 focus:outline-none focus:ring-2 \
         focus:ring-red-500 focus:ring-offset-2"
    } else {
        "inline-flex items-center rounded-md bg-brand-primary px-4 py-2 text-sm \
         font-medium text-white shadow-sm hover:bg-brand-primary-dark focus:outline-none \
         focus:ring-2 focus:ring-brand-primary focus:ring-offset-2"
    };
    html! {
        button type="submit" class=(classes) {
            @if entity.is_empty() {
                (action)
            } @else {
                (action) " " (entity)
            }
        }
    }
}
