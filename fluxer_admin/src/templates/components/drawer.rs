// SPDX-License-Identifier: AGPL-3.0-or-later

use maud::{Markup, PreEscaped, html};

use super::icons::{close_icon, spinner_icon};

#[derive(Clone, Copy)]
pub enum DrawerSide {
    Right,
}

#[derive(Clone, Copy)]
pub enum DrawerWidth {
    Lg,
    Xl,
}

fn width_class(width: DrawerWidth) -> &'static str {
    match width {
        DrawerWidth::Lg => "sm:max-w-lg",
        DrawerWidth::Xl => "sm:max-w-xl",
    }
}

fn side_class(side: DrawerSide) -> &'static str {
    match side {
        DrawerSide::Right => "right-0 top-0 h-[100dvh] w-full sm:w-[92vw]",
    }
}

fn side_off_class(side: DrawerSide) -> &'static str {
    match side {
        DrawerSide::Right => "translate-x-full",
    }
}

fn side_name(side: DrawerSide) -> &'static str {
    match side {
        DrawerSide::Right => "right",
    }
}

pub fn drawer(
    id: &str,
    title: &str,
    description: Option<&str>,
    side: DrawerSide,
    width: DrawerWidth,
    footer: Option<Markup>,
    body: Option<Markup>,
) -> Markup {
    let title_id = format!("{id}-title");
    let desc_id = format!("{id}-description");
    let body_id = format!("{id}-body");
    let panel_classes = format!(
        "drawer-panel pointer-events-auto fixed z-50 flex flex-col \
         bg-white shadow-2xl {} {} {}",
        side_class(side),
        width_class(width),
        side_off_class(side),
    );
    html! {
        div data-drawer-root=(id) class="contents" {
            div data-drawer-overlay=(id)
                class="drawer-overlay pointer-events-none fixed inset-0 z-40 \
                       bg-black/50 opacity-0"
                aria-hidden="true" {}
            aside id=(id)
                  popover="auto"
                  data-drawer-panel=(id)
                  data-drawer-side=(side_name(side))
                  class=(panel_classes)
                  role="dialog" aria-modal="true"
                  aria-labelledby=(title_id)
                  aria-describedby=[description.map(|_| desc_id.as_str())]
                  tabindex="-1"
            {
                header class="flex items-start justify-between gap-3 \
                              border-neutral-200 border-b px-4 py-4 sm:px-6" {
                    div class="min-w-0 flex-1" {
                        h2 id=(title_id)
                           class="break-words font-semibold text-lg text-neutral-900"
                           data-drawer-title="true" {
                            (title)
                        }
                        @if let Some(desc) = description {
                            p id=(desc_id)
                              class="mt-1 text-neutral-500 text-sm"
                              data-drawer-description="true" {
                                (desc)
                            }
                        }
                    }
                    button type="button" data-drawer-close=(id)
                           popovertarget=(id) popovertargetaction="hide"
                           aria-label="Close drawer"
                           class="inline-flex h-11 w-11 flex-shrink-0 items-center \
                                  justify-center rounded-md text-neutral-500 \
                                  transition-colors hover:bg-neutral-100 \
                                  hover:text-neutral-900 focus:outline-none \
                                  focus-visible:ring-2 focus-visible:ring-brand-primary"
                    {
                        (close_icon())
                    }
                }
                div id=(body_id) data-drawer-body=(id)
                    class="drawer-scroll flex-1 overflow-y-auto px-4 py-4 sm:px-6 sm:py-6"
                {
                    @if let Some(body_content) = body {
                        (body_content)
                    } @else {
                        (drawer_loading_state())
                    }
                }
                @if let Some(footer_content) = footer {
                    footer class="flex flex-shrink-0 flex-wrap items-center justify-end \
                                  gap-2 border-neutral-200 border-t bg-neutral-50 \
                                  px-4 py-3 sm:px-6" {
                        (footer_content)
                    }
                }
            }
        }
    }
}

pub fn drawer_loading_state() -> Markup {
    html! {
        div role="status" aria-live="polite"
            class="flex flex-col items-center justify-center gap-3 py-12 \
                   text-neutral-500" {
            (spinner_icon())
            span class="text-sm" { "Loading\u{2026}" }
        }
    }
}

pub fn drawer_controller_script() -> Markup {
    html! {
        script defer { (PreEscaped(DRAWER_CONTROLLER_SCRIPT)) }
    }
}

const DRAWER_CONTROLLER_SCRIPT: &str = r#"
(function () {
	var FOCUSABLE = 'a[href], button:not([disabled]), input:not([disabled]), select:not([disabled]), textarea:not([disabled]), [tabindex]:not([tabindex="-1"])';
	var activeDrawer = null;
	var lastTrigger = null;
	function panel(id) {
		return document.getElementById(id);
	}
	function overlay(id) {
		return document.querySelector('[data-drawer-overlay="' + id + '"]');
	}
	function body(id) {
		return document.querySelector('[data-drawer-body="' + id + '"]');
	}
	function title(id) {
		return document.querySelector('[data-drawer-panel="' + id + '"] [data-drawer-title]');
	}
	function setLoading(id) {
		var target = body(id);
		if (!target) return;
		target.innerHTML = '<div role="status" aria-live="polite" class="flex flex-col items-center justify-center gap-3 py-12 text-neutral-500"><span class="text-sm">Loading...</span></div>';
	}
	function setError(id) {
		var target = body(id);
		if (!target) return;
		target.innerHTML = '<div role="alert" class="rounded-lg border border-red-200 bg-red-50 p-4 text-red-800 text-sm">Failed to load drawer content.</div>';
	}
	function openDrawer(id, trigger) {
		var el = panel(id);
		if (!el) return false;
		lastTrigger = trigger || document.activeElement;
		activeDrawer = id;
		var ov = overlay(id);
		el.setAttribute('data-open', 'true');
		el.setAttribute('aria-hidden', 'false');
		if (typeof el.showPopover === 'function') {
			try { el.showPopover(); } catch (e) {}
		}
		if (ov) {
			ov.classList.remove('pointer-events-none', 'opacity-0');
			ov.classList.add('opacity-100');
		}
		document.body.classList.add('drawer-open');
		window.requestAnimationFrame(function () {
			var focusTarget = el.querySelector(FOCUSABLE) || el;
			if (focusTarget && typeof focusTarget.focus === 'function') focusTarget.focus();
		});
		return true;
	}
	function closeDrawer(id) {
		var drawerId = id || activeDrawer;
		var el = drawerId ? panel(drawerId) : null;
		if (!el) return;
		var ov = overlay(drawerId);
		el.removeAttribute('data-open');
		el.setAttribute('aria-hidden', 'true');
		if (typeof el.hidePopover === 'function') {
			try { el.hidePopover(); } catch (e) {}
		}
		if (ov) {
			ov.classList.add('pointer-events-none', 'opacity-0');
			ov.classList.remove('opacity-100');
		}
		document.body.classList.remove('drawer-open');
		activeDrawer = null;
		if (lastTrigger && typeof lastTrigger.focus === 'function') lastTrigger.focus();
	}
	async function loadDrawer(trigger, id) {
		var href = trigger.getAttribute('data-drawer-href') || trigger.getAttribute('hx-get') || trigger.getAttribute('href');
		if (!href) return;
		setLoading(id);
		try {
			var response = await fetch(href, {credentials: 'same-origin', headers: {'X-Requested-With': 'fetch'}});
			if (!response.ok) throw new Error('HTTP ' + response.status);
			var target = body(id);
			if (target) target.innerHTML = await response.text();
		} catch (e) {
			setError(id);
		}
	}
	document.addEventListener('click', function (event) {
		var target = event.target;
		if (!(target instanceof Element)) return;
		var trigger = target.closest('[data-drawer-open]');
		if (trigger instanceof HTMLElement) {
			var id = trigger.getAttribute('data-drawer-open');
			if (!id || !panel(id)) return;
			event.preventDefault();
			event.stopImmediatePropagation();
			var label = trigger.getAttribute('data-drawer-title');
			var titleEl = title(id);
			if (label && titleEl) titleEl.textContent = label;
			openDrawer(id, trigger);
			loadDrawer(trigger, id);
			return;
		}
		var close = target.closest('[data-drawer-close]');
		if (close instanceof HTMLElement) {
			event.preventDefault();
			closeDrawer(close.getAttribute('data-drawer-close'));
			return;
		}
		var ov = target.closest('[data-drawer-overlay]');
		if (ov instanceof HTMLElement) {
			event.preventDefault();
			closeDrawer(ov.getAttribute('data-drawer-overlay'));
		}
	}, true);
	document.addEventListener('keydown', function (event) {
		if (!activeDrawer) return;
		if (event.key === 'Escape') {
			event.preventDefault();
			closeDrawer(activeDrawer);
			return;
		}
		if (event.key !== 'Tab') return;
		var el = panel(activeDrawer);
		if (!el) return;
		var focusables = Array.prototype.filter.call(el.querySelectorAll(FOCUSABLE), function (item) {
			return item instanceof HTMLElement && item.offsetParent !== null;
		});
		if (focusables.length === 0) return;
		var first = focusables[0];
		var last = focusables[focusables.length - 1];
		if (event.shiftKey && document.activeElement === first) {
			event.preventDefault();
			last.focus();
		} else if (!event.shiftKey && document.activeElement === last) {
			event.preventDefault();
			first.focus();
		}
	});
})();
"#;
