// SPDX-License-Identifier: AGPL-3.0-or-later

pub const ACTIVITY_CARD_SCRIPT: &str = r#"
(function () {
	if (window.__fluxerAdminActivityCards) return;
	window.__fluxerAdminActivityCards = true;
	var detailsState = Object.create(null);

	function normalizeTimestamp(value) {
		var parsed = Number(value);
		if (!Number.isFinite(parsed) || parsed <= 0) return null;
		if (parsed > 10000000000) return Math.floor(parsed / 1000);
		return Math.floor(parsed);
	}

	function formatDuration(totalSeconds) {
		var hours = Math.floor(totalSeconds / 3600);
		var minutes = Math.floor((totalSeconds % 3600) / 60);
		var seconds = totalSeconds % 60;
		if (hours > 0) return hours + ':' + String(minutes).padStart(2, '0') + ':' + String(seconds).padStart(2, '0');
		return minutes + ':' + String(seconds).padStart(2, '0');
	}

	function updateTimers(nowSeconds) {
		document.querySelectorAll('[data-activity-timer]').forEach(function (el) {
			var start = normalizeTimestamp(el.getAttribute('data-start'));
			var end = normalizeTimestamp(el.getAttribute('data-end'));
			var text = '';
			if (start && end && end > start) {
				text = formatDuration(Math.max(0, nowSeconds - start)) + ' elapsed • ' + formatDuration(Math.max(0, end - nowSeconds)) + ' left';
			} else if (start) {
				text = formatDuration(Math.max(0, nowSeconds - start)) + ' elapsed';
			} else if (end && end > nowSeconds) {
				text = formatDuration(Math.max(0, end - nowSeconds)) + ' left';
			}
			el.textContent = text;
		});
	}

	function updateProgress(nowSeconds) {
		document.querySelectorAll('[data-activity-progress]').forEach(function (el) {
			var start = normalizeTimestamp(el.getAttribute('data-start'));
			var end = normalizeTimestamp(el.getAttribute('data-end'));
			if (!start || !end || end <= start) {
				el.style.width = '0%';
				return;
			}
			var elapsed = Math.max(0, Math.min(end - start, nowSeconds - start));
			el.style.width = String((elapsed / (end - start)) * 100) + '%';
		});
	}

	function update() {
		var nowSeconds = Math.floor(Date.now() / 1000);
		updateTimers(nowSeconds);
		updateProgress(nowSeconds);
	}

	function bindDetails(root) {
		var scope = root && root.querySelectorAll ? root : document;
		scope.querySelectorAll('[data-activity-raw]').forEach(function (el) {
			var key = el.getAttribute('data-activity-raw');
			if (!key) return;
			if (detailsState[key] === true) el.open = true;
			if (el.dataset.activityRawBound === 'true') return;
			el.dataset.activityRawBound = 'true';
			el.addEventListener('toggle', function () {
				detailsState[key] = el.open;
			});
		});
	}

	if (document.readyState === 'loading') {
		document.addEventListener('DOMContentLoaded', function () {
			bindDetails(document);
			update();
		});
	} else {
		bindDetails(document);
		update();
	}
	document.body.addEventListener('htmx:load', function (event) {
		bindDetails(event.detail && event.detail.elt ? event.detail.elt : event.target);
		update();
	});
	window.setInterval(update, 1000);
})();
"#;
