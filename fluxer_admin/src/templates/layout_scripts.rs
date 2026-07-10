// SPDX-License-Identifier: AGPL-3.0-or-later

pub const SIDEBAR_ACTIVE_SCROLL_SCRIPT: &str = r#"
(function () {
	var sidebar = document.querySelector('[data-sidebar]');
	if (!sidebar) return;
	var scroller = sidebar.querySelector('[data-sidebar-nav]') || sidebar;
	var key = 'fluxer-admin-sidebar-scroll';
	var restored = false;
	try {
		var saved = sessionStorage.getItem(key);
		if (saved !== null) {
			scroller.scrollTop = Number(saved) || 0;
			restored = true;
		}
	} catch (e) {}
	if (!restored) {
		var el = sidebar.querySelector('[data-active]');
		if (el) el.scrollIntoView({block: 'nearest'});
	}
	if (scroller.dataset.sidebarScrollBound === 'true') return;
	scroller.dataset.sidebarScrollBound = 'true';
	scroller.addEventListener('scroll', function () {
		try { sessionStorage.setItem(key, String(scroller.scrollTop)); } catch (e) {}
	}, {passive: true});
	sidebar.addEventListener('click', function () {
		try { sessionStorage.setItem(key, String(scroller.scrollTop)); } catch (e) {}
	}, true);
})();

"#;

pub const SIDEBAR_SCRIPT: &str = r#"
(function () {
	var sidebar = document.querySelector('[data-sidebar]');
	var overlay = document.querySelector('[data-sidebar-overlay]');
	var toggles = document.querySelectorAll('[data-sidebar-toggle]');
	var closes = document.querySelectorAll('[data-sidebar-close]');
	if (!sidebar || !overlay) return;
	var lastFocusedToggle = null;
	var FOCUSABLE = 'a[href], button:not([disabled]), [tabindex]:not([tabindex="-1"])';
	function isDesktop() { return window.innerWidth >= 1024; }
	function setExpanded(expanded) {
		toggles.forEach(function (btn) { btn.setAttribute('aria-expanded', expanded ? 'true' : 'false'); });
	}
	function open(triggeredBy) {
		if (isDesktop()) return;
		lastFocusedToggle = triggeredBy || document.activeElement;
		sidebar.classList.remove('-translate-x-full');
		sidebar.setAttribute('aria-hidden', 'false');
		overlay.classList.remove('opacity-0', 'pointer-events-none');
		overlay.classList.add('opacity-100');
		document.body.classList.add('drawer-open');
		setExpanded(true);
		var first = sidebar.querySelector(FOCUSABLE);
		if (first) first.focus();
	}
	function close() {
		if (isDesktop()) return;
		sidebar.classList.add('-translate-x-full');
		sidebar.setAttribute('aria-hidden', 'true');
		overlay.classList.add('opacity-0', 'pointer-events-none');
		overlay.classList.remove('opacity-100');
		document.body.classList.remove('drawer-open');
		setExpanded(false);
		if (lastFocusedToggle && typeof lastFocusedToggle.focus === 'function') {
			lastFocusedToggle.focus();
		}
	}
	function isOpen() {
		return !sidebar.classList.contains('-translate-x-full') && !isDesktop();
	}
	toggles.forEach(function (btn) {
		btn.addEventListener('click', function () {
			if (sidebar.classList.contains('-translate-x-full')) {
				open(btn);
			} else {
				close();
			}
		});
	});
	closes.forEach(function (btn) {
		btn.addEventListener('click', close);
	});
	overlay.addEventListener('click', close);
	document.addEventListener('keydown', function (event) {
		if (!isOpen()) return;
		if (event.key === 'Escape') {
			event.preventDefault();
			close();
			return;
		}
		if (event.key === 'Tab') {
			var focusables = Array.prototype.filter.call(
				sidebar.querySelectorAll(FOCUSABLE),
				function (el) { return !el.hasAttribute('disabled') && el.offsetParent !== null; }
			);
			if (focusables.length === 0) return;
			var first = focusables[0];
			var last = focusables[focusables.length - 1];
			var active = document.activeElement;
			if (event.shiftKey && active === first) {
				event.preventDefault();
				last.focus();
			} else if (!event.shiftKey && active === last) {
				event.preventDefault();
				first.focus();
			} else if (!sidebar.contains(active)) {
				event.preventDefault();
				first.focus();
			}
		}
	});
	function syncForDesktop() {
		if (isDesktop()) {
			overlay.classList.add('opacity-0', 'pointer-events-none');
			overlay.classList.remove('opacity-100');
			document.body.classList.remove('drawer-open');
			sidebar.classList.remove('-translate-x-full');
			sidebar.setAttribute('aria-hidden', 'false');
			setExpanded(false);
		} else {
			sidebar.classList.add('-translate-x-full');
			sidebar.setAttribute('aria-hidden', 'true');
			setExpanded(false);
		}
	}
	window.addEventListener('resize', syncForDesktop);
	syncForDesktop();
})();

"#;

pub const ADMIN_ACTION_FORM_SCRIPT: &str = r#"
(function () {
	if (window.__fluxerAdminActionForms) return;
	window.__fluxerAdminActionForms = true;

	function basePath() {
		return document.documentElement.getAttribute('data-base-path') || '';
	}

	function stripBase(pathname) {
		var base = basePath();
		if (base && pathname.indexOf(base + '/') === 0) return pathname.slice(base.length);
		if (base && pathname === base) return '/';
		return pathname;
	}

	function formUrl(form) {
		var raw = form.getAttribute('action') || window.location.href;
		try {
			return new URL(raw, window.location.href);
		} catch (error) {
			return null;
		}
	}

	function existingTarget(form) {
		return (form.getAttribute('hx-target') || form.getAttribute('data-hx-target') || '').trim();
	}

	function isResultForm(form, url) {
		if (form.dataset.adminResultForm === 'true' || form.dataset.adminAllowSwap === 'true') return true;
		if (form.dataset.adminToast === 'false') return true;
		if ((form.getAttribute('hx-boost') || '').toLowerCase() === 'false') return true;
		if (form.hasAttribute('target')) return true;
		var target = existingTarget(form);
		if (target && target !== '#flash-container' && target !== 'flash-container') return true;
		var enctype = (form.getAttribute('enctype') || '').toLowerCase();
		if (enctype === 'multipart/form-data') return true;
		if (!url || url.origin !== window.location.origin) return true;
		var path = stripBase(url.pathname);
		var action = url.searchParams.get('action') || '';
		if (path === '/gift-codes') return true;
		if (path === '/logout') return true;
		if (path === '/messages' && (
			action === 'search' ||
			action === 'browse' ||
			action === 'lookup' ||
			action === 'lookup-by-attachment'
		)) return true;
		return false;
	}

	function decorateForm(form) {
		if (!(form instanceof HTMLFormElement)) return;
		if (form.dataset.adminActionFormProcessed === 'true') return;
		var method = (form.getAttribute('method') || 'get').toLowerCase();
		if (method !== 'post') return;
		var url = formUrl(form);
		if (isResultForm(form, url)) return;
		var action = form.getAttribute('action') || window.location.pathname + window.location.search;
		form.setAttribute('hx-post', action);
		form.setAttribute('hx-target', '#flash-container');
		form.setAttribute('hx-swap', 'none');
		form.setAttribute('hx-push-url', 'false');
		form.dataset.adminActionFormProcessed = 'true';
		if (window.htmx && typeof window.htmx.process === 'function') {
			window.htmx.process(form);
		}
	}

	function decorate(root) {
		var scope = root && root.querySelectorAll ? root : document;
		if (scope instanceof HTMLFormElement) {
			decorateForm(scope);
			return;
		}
		Array.prototype.forEach.call(scope.querySelectorAll('form'), decorateForm);
	}

	if (document.readyState === 'loading') {
		document.addEventListener('DOMContentLoaded', function () { decorate(document); });
	} else {
		decorate(document);
	}
	document.body.addEventListener('htmx:load', function (event) {
		decorate(event.detail && event.detail.elt ? event.detail.elt : event.target);
	});
})();

"#;

pub const HTMX_SCROLL_PRESERVER_SCRIPT: &str = r#"
(function () {
	if (window.__fluxerAdminHtmxScrollPreserver) return;
	window.__fluxerAdminHtmxScrollPreserver = true;
	var pendingKey = '__fluxerAdminPendingHtmxScroll';
	var restoreTimer = 0;

	function adminScroller() {
		var main = document.getElementById('main-content');
		if (!main || !main.parentElement) return null;
		return main.parentElement;
	}

	function shouldPreserve(detail) {
		var cfg = detail && detail.requestConfig;
		if (!cfg) return false;
		var verb = String(cfg.verb || '').toLowerCase();
		return verb !== '' && verb !== 'get';
	}

	function snapshot() {
		var scroller = adminScroller();
		window[pendingKey] = {
			top: scroller ? scroller.scrollTop : null,
			left: scroller ? scroller.scrollLeft : null,
			windowX: window.scrollX || window.pageXOffset || 0,
			windowY: window.scrollY || window.pageYOffset || 0
		};
	}

	function restore() {
		var saved = window[pendingKey];
		if (!saved) return;
		var scroller = adminScroller();
		if (scroller && typeof saved.top === 'number') {
			scroller.scrollTop = saved.top;
			scroller.scrollLeft = saved.left || 0;
		}
		if (typeof window.scrollTo === 'function' && typeof saved.windowY === 'number') {
			window.scrollTo(saved.windowX || 0, saved.windowY || 0);
		}
	}

	function scheduleRestore(delay) {
		if (!window[pendingKey]) return;
		if (restoreTimer) window.clearTimeout(restoreTimer);
		restoreTimer = window.setTimeout(function () {
			var frames = 0;
			function tick() {
				restore();
				frames += 1;
				if (frames < 4 && typeof window.requestAnimationFrame === 'function') {
					window.requestAnimationFrame(tick);
				} else {
					window[pendingKey] = null;
				}
			}
			if (typeof window.requestAnimationFrame === 'function') {
				window.requestAnimationFrame(tick);
			} else {
				restore();
				window[pendingKey] = null;
			}
		}, delay);
	}

	document.addEventListener('htmx:beforeRequest', function (event) {
		if (shouldPreserve(event.detail)) snapshot();
	}, true);

	document.addEventListener('htmx:afterSettle', function (event) {
		if (shouldPreserve(event.detail)) scheduleRestore(0);
	}, true);

	document.addEventListener('htmx:afterRequest', function (event) {
		if (shouldPreserve(event.detail)) scheduleRestore(80);
	}, true);
})();

"#;

pub const HTMX_FLASH_SCRIPT: &str = r#"
(function () {
	if (window.__fluxerAdminToastScript) return;
	window.__fluxerAdminToastScript = true;

	var activeToast = null;
	var hideTimer = 0;

	function palette(level) {
		if (level === 'success') {
			return {background: '#ecfdf3', border: '#bbf7d0', color: '#166534'};
		}
		if (level === 'error') {
			return {background: '#fef2f2', border: '#fecaca', color: '#991b1b'};
		}
		return {background: '#eff6ff', border: '#bfdbfe', color: '#1e40af'};
	}

	function stack() {
		var el = document.getElementById('admin-toast-stack');
		if (el) return el;
		el = document.createElement('div');
		el.id = 'admin-toast-stack';
		el.setAttribute('aria-live', 'polite');
		el.setAttribute('aria-atomic', 'true');
		el.style.cssText = [
			'position:fixed',
			'top:16px',
			'left:50%',
			'transform:translateX(-50%)',
			'z-index:9999',
			'display:flex',
			'flex-direction:column',
			'align-items:center',
			'gap:8px',
			'width:min(calc(100vw - 24px), 520px)',
			'pointer-events:none'
		].join(';');
		document.body.appendChild(el);
		return el;
	}

	function showToast(level, message, options) {
		options = options || {};
		if (!message) message = level === 'error' ? 'Action failed.' : level === 'success' ? 'Done.' : 'Working...';
		if (hideTimer) {
			window.clearTimeout(hideTimer);
			hideTimer = 0;
		}
		if (activeToast && !activeToast.isConnected) activeToast = null;
		if (!activeToast) {
			activeToast = document.createElement('div');
			activeToast.setAttribute('role', 'status');
			activeToast.style.cssText = [
				'max-width:100%',
				'border:1px solid',
				'border-radius:8px',
				'box-shadow:0 12px 28px rgba(15, 23, 42, 0.16)',
				'font:500 14px/1.4 system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif',
				'padding:10px 14px',
				'pointer-events:auto',
				'transition:opacity 120ms ease, transform 120ms ease',
				'white-space:normal'
			].join(';');
			stack().appendChild(activeToast);
		}
		var colors = palette(level);
		activeToast.textContent = message;
		activeToast.style.background = colors.background;
		activeToast.style.borderColor = colors.border;
		activeToast.style.color = colors.color;
		activeToast.style.opacity = '1';
		activeToast.style.transform = 'translateY(0)';
		if (!options.persist) {
			hideTimer = window.setTimeout(function () {
				if (!activeToast) return;
				activeToast.style.opacity = '0';
				activeToast.style.transform = 'translateY(-4px)';
				window.setTimeout(function () {
					if (activeToast) activeToast.remove();
					activeToast = null;
				}, 160);
			}, options.duration || 5000);
		}
	}

	function requestVerb(detail) {
		var cfg = detail && detail.requestConfig;
		return String((cfg && cfg.verb) || '').toLowerCase();
	}

	function isMutation(detail) {
		var verb = requestVerb(detail);
		return verb !== '' && verb !== 'get';
	}

	function targetIsFlashContainer(detail) {
		var target = detail && detail.target;
		return !!target && target.id === 'flash-container';
	}

	function markToastHandled(detail) {
		if (detail) detail.__fluxerToastHandled = true;
		if (detail && detail.xhr) detail.xhr.__fluxerToastHandled = true;
	}

	function toastWasHandled(detail) {
		return !!(detail && (detail.__fluxerToastHandled || (detail.xhr && detail.xhr.__fluxerToastHandled)));
	}

	function mutationElement(detail) {
		if (!detail) return null;
		return detail.elt || (detail.requestConfig && detail.requestConfig.elt) || null;
	}

	function mutationControls(detail) {
		var elt = mutationElement(detail);
		if (!elt || !elt.querySelectorAll) return [];
		var root = elt;
		if (elt.tagName && elt.tagName.toLowerCase() !== 'form' && elt.closest) {
			root = elt.closest('form') || elt;
		}
		var controls = [];
		if (root.matches && root.matches('button,input,select,textarea')) {
			controls.push(root);
		}
		if (root.querySelectorAll) {
			Array.prototype.forEach.call(root.querySelectorAll('button,input,select,textarea'), function (control) {
				controls.push(control);
			});
		}
		return controls;
	}

	function disableMutationControls(detail) {
		if (!isMutation(detail) || toastWasHandled(detail)) return;
		var disabled = [];
		mutationControls(detail).forEach(function (control) {
			if (control.disabled || control.dataset.fluxerAdminRequestDisabled === 'true') return;
			control.disabled = true;
			control.dataset.fluxerAdminRequestDisabled = 'true';
			disabled.push(control);
		});
		if (detail) detail.__fluxerDisabledControls = disabled;
		if (detail && detail.xhr) detail.xhr.__fluxerDisabledControls = disabled;
	}

	function restoreMutationControls(detail) {
		var controls = (detail && detail.__fluxerDisabledControls) ||
			(detail && detail.xhr && detail.xhr.__fluxerDisabledControls) ||
			[];
		controls.forEach(function (control) {
			if (control.dataset.fluxerAdminRequestDisabled !== 'true') return;
			control.disabled = false;
			delete control.dataset.fluxerAdminRequestDisabled;
		});
	}

	function responseUrlPath(xhr) {
		if (!xhr || !xhr.responseURL) return '';
		try {
			return new URL(xhr.responseURL, window.location.href).pathname;
		} catch (error) {
			return '';
		}
	}

	function isLoginResponse(xhr) {
		var base = document.documentElement.getAttribute('data-base-path') || '';
		return responseUrlPath(xhr) === base + '/login';
	}

	function parseFlashResponse(responseText, status) {
		var root = document.createElement('div');
		root.innerHTML = responseText || '';
		var flash = root.querySelector('#flash-container');
		var source = flash || root.firstElementChild || root;
		var text = (source.textContent || '').replace(/\s+/g, ' ').trim();
		var className = '';
		var element = source instanceof Element ? source : root.firstElementChild;
		var depth = 0;
		while (element && depth < 4) {
			className += ' ' + (element.getAttribute('class') || '');
			element = element.firstElementChild;
			depth += 1;
		}
		var level = status >= 400 ? 'error' : 'success';
		if (className.indexOf('red-') >= 0) level = 'error';
		if (className.indexOf('green-') >= 0) level = 'success';
		if (className.indexOf('blue-') >= 0) level = 'info';
		return {
			level: level,
			message: text || (level === 'error' ? 'Action failed.' : 'Done.')
		};
	}

	function parseAdminToastHeader(xhr) {
		if (!xhr || typeof xhr.getResponseHeader !== 'function') return null;
		var raw = xhr.getResponseHeader('X-Fluxer-Admin-Toast');
		if (!raw) return null;
		try {
			var parsed = JSON.parse(raw);
			return {
				level: parsed && parsed.level ? String(parsed.level) : 'info',
				message: parsed && parsed.message ? String(parsed.message) : ''
			};
		} catch (error) {
			return null;
		}
	}

	document.body.addEventListener('showFlash', function (evt) {
		var d = evt.detail || {};
		showToast(d.level || 'info', d.message || '');
	});

	document.addEventListener('htmx:beforeRequest', function (event) {
		if (!isMutation(event.detail)) return;
		showToast('info', 'Working...', {persist: true});
	}, true);

	document.addEventListener('htmx:beforeSend', function (event) {
		disableMutationControls(event.detail);
	}, true);

	document.addEventListener('htmx:beforeSwap', function (event) {
		if (!isMutation(event.detail) || !targetIsFlashContainer(event.detail)) return;
		var xhr = event.detail.xhr;
		var parsed = isLoginResponse(xhr)
			? {level: 'error', message: 'Session expired. Sign in again.'}
			: parseFlashResponse(xhr ? xhr.responseText : '', xhr ? xhr.status : 200);
		event.detail.shouldSwap = false;
		event.detail.isError = false;
		markToastHandled(event.detail);
		showToast(parsed.level, parsed.message);
	}, true);

	document.addEventListener('htmx:afterRequest', function (event) {
		restoreMutationControls(event.detail);
		if (!isMutation(event.detail) || toastWasHandled(event.detail)) return;
		var xhr = event.detail.xhr;
		var serverToast = parseAdminToastHeader(xhr);
		if (serverToast) {
			showToast(serverToast.level, serverToast.message);
			markToastHandled(event.detail);
			return;
		}
		var failed = !event.detail.successful || (xhr && xhr.status >= 400) || isLoginResponse(xhr);
		showToast(failed ? 'error' : 'success', isLoginResponse(xhr) ? 'Session expired. Sign in again.' : failed ? 'Action failed.' : 'Done.');
		markToastHandled(event.detail);
	}, true);
})();
"#;

pub const COPY_TO_CLIPBOARD_SCRIPT: &str = r#"
window.__adminCopyToClipboard = function (text, btn, successLabel) {
	if (!navigator.clipboard || !btn) return;
	navigator.clipboard.writeText(text).then(function () {
		var original = btn.textContent;
		btn.textContent = successLabel || 'Copied!';
		setTimeout(function () { btn.textContent = original; }, 1500);
	});
};
"#;

pub const ARCHIVE_POLL_SCRIPT: &str = r#"
(function () {
	var rows = document.querySelectorAll('tr[data-archive-id]');
	var hasPending = false;
	for (var i = 0; i < rows.length; i++) {
		var status = rows[i].querySelector('.archive-status');
		if (status && status.textContent.indexOf('Completed') === -1 && status.textContent.indexOf('Failed') === -1) {
			hasPending = true;
			break;
		}
	}
	if (hasPending) {
		setTimeout(function () {
			window.location.reload();
		}, 5000);
	}
})();
"#;

pub const SH_LINK_REWRITE_SCRIPT: &str = r#"
(function () {
	if (window.location.search.indexOf('sh=1') === -1) return;
	function rewriteHref(el) {
		var href = el.getAttribute('href');
		if (!href || href.indexOf('sh=1') >= 0) return;
		if (href.charAt(0) === '#' || href.indexOf('javascript:') === 0 || href.indexOf('data:') === 0 || href.indexOf('mailto:') === 0) return;
		if (href.indexOf('://') >= 0) {
			try {
				var url = new URL(href);
				if (url.origin !== window.location.origin) return;
			} catch (e) {
				return;
			}
		}
		var sep = href.indexOf('?') >= 0 ? '&' : '?';
		el.setAttribute('href', href + sep + 'sh=1');
	}
	function rewriteAction(form) {
		var action = form.getAttribute('action');
		if (!action || action.indexOf('sh=1') >= 0) return;
		var sep = action.indexOf('?') >= 0 ? '&' : '?';
		form.setAttribute('action', action + sep + 'sh=1');
	}
	document.querySelectorAll('a[href]').forEach(rewriteHref);
	document.querySelectorAll('form[action]').forEach(rewriteAction);
	document.addEventListener('click', function (e) {
		var a = e.target.closest('a[href]');
		if (a) rewriteHref(a);
	}, true);
	document.addEventListener('submit', function (e) {
		var form = e.target.closest('form[action]');
		if (form) rewriteAction(form);
	}, true);
})();

"#;
