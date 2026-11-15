// SPDX-License-Identifier: AGPL-3.0-or-later

use maud::{Markup, PreEscaped, html};

pub struct DetailTab {
    pub id: String,
    pub label: String,
    pub href: String,
    pub tab_url: String,
    pub active: bool,
}

pub fn build_detail_tabs(
    tabs: &[(&str, &str)],
    base_url: &str,
    entity_id: &str,
    active_tab: &str,
    visible: impl Fn(&str) -> bool,
) -> Vec<DetailTab> {
    tabs.iter()
        .filter(|(id, _)| visible(id))
        .map(|(id, label)| DetailTab {
            id: id.to_string(),
            label: label.to_string(),
            tab_url: format!("{base_url}/{entity_id}/tabs/{id}"),
            href: detail_page_href(base_url, entity_id, id),
            active: *id == active_tab,
        })
        .collect()
}

pub fn detail_page_href(base_url: &str, entity_id: &str, tab: &str) -> String {
    if tab == "overview" {
        format!("{base_url}/{entity_id}")
    } else {
        format!("{base_url}/{entity_id}?tab={tab}")
    }
}

const MOBILE_TAB_BASE: &str = "inline-flex min-h-[44px] flex-shrink-0 items-center \
    whitespace-nowrap rounded-md px-3 py-2 font-medium text-sm transition-colors \
    focus:outline-none focus-visible:ring-2 focus-visible:ring-brand-primary \
    focus-visible:ring-offset-2";

const DESKTOP_TAB_BASE: &str = "flex min-h-[44px] items-center border-l-2 py-2 pl-3 \
    pr-2 font-medium text-sm transition-colors focus:outline-none \
    focus-visible:ring-2 focus-visible:ring-brand-primary focus-visible:ring-inset";

pub fn detail_tabs(tabs: &[DetailTab], aria_label: &str, target_id: &str) -> Markup {
    let hx_target = format!("#{target_id}");
    html! {
        div class="-mx-3 sm:mx-0 lg:hidden" {
            nav aria-label=(aria_label) data-detail-tabs-mobile=""
                class="table-scroll flex gap-1 overflow-x-auto px-3 pb-1" {
                @for tab in tabs {
                    @let mobile_class = if tab.active {
                        format!("{MOBILE_TAB_BASE} bg-neutral-900 text-white")
                    } else {
                        format!(
                            "{MOBILE_TAB_BASE} text-neutral-600 \
                             hover:bg-neutral-100 hover:text-neutral-900"
                        )
                    };
                    a href=(tab.href)
                      hx-get=(tab.tab_url)
                      hx-target=(hx_target)
                      hx-push-url=(tab.href)
                      hx-swap="innerHTML"
                      aria-current=[tab.active.then_some("page")]
                      data-tab-id=(tab.id)
                      data-active[tab.active]
                      class=(mobile_class) {
                        (tab.label)
                    }
                }
            }
        }
        nav aria-label=(aria_label)
            class="hidden w-48 shrink-0 flex-col gap-1 border-neutral-200 \
                   border-r pr-4 lg:flex" {
            @for tab in tabs {
                @let desktop_class = if tab.active {
                    format!("{DESKTOP_TAB_BASE} border-neutral-900 text-neutral-900")
                } else {
                    format!(
                        "{DESKTOP_TAB_BASE} border-transparent text-neutral-500 \
                         hover:border-neutral-300 hover:text-neutral-700"
                    )
                };
                a href=(tab.href)
                  hx-get=(tab.tab_url)
                  hx-target=(hx_target)
                  hx-push-url=(tab.href)
                  hx-swap="innerHTML"
                  aria-current=[tab.active.then_some("page")]
                  data-tab-id=(tab.id)
                  class=(desktop_class) {
                    (tab.label)
                }
            }
        }
        script defer {
            (PreEscaped(DETAIL_TABS_SCRIPT))
        }
    }
}

const DETAIL_TABS_SCRIPT: &str = r#"
(function () {
	var lists = document.querySelectorAll('[data-detail-tabs-mobile]');
	for (var i = 0; i < lists.length; i++) {
		var active = lists[i].querySelector('[data-active]');
		if (active && typeof active.scrollIntoView === 'function') {
			active.scrollIntoView({block: 'nearest', inline: 'center'});
		}
	}
	document.body.addEventListener('htmx:afterOnLoad', function (e) {
		var link = e.detail.elt;
		var tabId = link && link.getAttribute('data-tab-id');
		if (!tabId) return;
		var all = document.querySelectorAll('[data-tab-id]');
		for (var i = 0; i < all.length; i++) {
			var a = all[i];
			var isActive = a.getAttribute('data-tab-id') === tabId;
			if (isActive) {
				a.setAttribute('data-active', '');
				a.setAttribute('aria-current', 'page');
			} else {
				a.removeAttribute('data-active');
				a.removeAttribute('aria-current');
			}
			var isMobile = a.closest('[data-detail-tabs-mobile]');
			if (isMobile) {
				if (isActive) {
					a.className = a.className.replace(/text-neutral-600|hover:bg-neutral-100|hover:text-neutral-900/g, '').replace(/\s+/g, ' ').trim();
					if (a.className.indexOf('bg-neutral-900') === -1) a.className += ' bg-neutral-900 text-white';
				} else {
					a.className = a.className.replace(/bg-neutral-900|text-white/g, '').replace(/\s+/g, ' ').trim();
					if (a.className.indexOf('text-neutral-600') === -1) a.className += ' text-neutral-600 hover:bg-neutral-100 hover:text-neutral-900';
				}
			} else {
				if (isActive) {
					a.className = a.className.replace(/border-transparent|text-neutral-500|hover:border-neutral-300|hover:text-neutral-700/g, '').replace(/\s+/g, ' ').trim();
					if (a.className.indexOf('border-neutral-900') === -1) a.className += ' border-neutral-900 text-neutral-900';
				} else {
					a.className = a.className.replace(/border-neutral-900|text-neutral-900/g, '').replace(/\s+/g, ' ').trim();
					if (a.className.indexOf('border-transparent') === -1) a.className += ' border-transparent text-neutral-500 hover:border-neutral-300 hover:text-neutral-700';
				}
			}
		}
	});
})();
"#;
