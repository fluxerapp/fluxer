// SPDX-License-Identifier: AGPL-3.0-or-later

const HIGHLIGHT_BOX_LIMIT = 200;

export const APP_HIGHLIGHT_ATTRIBUTE = 'data-quick-css-highlight';

const HOST_STYLE = [
	'position: fixed !important',
	'inset: 0 !important',
	'width: 100vw !important',
	'height: 100vh !important',
	'margin: 0 !important',
	'padding: 0 !important',
	'border: none !important',
	'background: transparent !important',
	'overflow: visible !important',
	'pointer-events: none !important',
	'display: block !important',
	'visibility: visible !important',
	'opacity: 1 !important',
].join(';');

const BOX_STYLE = [
	'position: absolute',
	'box-sizing: border-box',
	'border: 2px solid var(--brand-primary, #4641d9)',
	'background: color-mix(in srgb, var(--brand-primary, #4641d9) 14%, transparent)',
	'border-radius: 2px',
].join(';');

export function excludeEditorPanel(host: HTMLElement): void {
	const panel = document.querySelector('[data-quick-css-panel]');
	const rect = panel?.getBoundingClientRect();
	if (!rect || rect.width === 0 || rect.height === 0) {
		host.style.setProperty('clip-path', 'none', 'important');
		return;
	}
	const width = innerWidth;
	const height = innerHeight;
	const {left, top, right, bottom} = rect;
	host.style.setProperty(
		'clip-path',
		`polygon(evenodd, 0 0, ${width}px 0, ${width}px ${height}px, 0 ${height}px, 0 0, ${left}px ${top}px, ${right}px ${top}px, ${right}px ${bottom}px, ${left}px ${bottom}px, ${left}px ${top}px)`,
		'important',
	);
}

export function showAppHighlights(elements: ReadonlyArray<Element>): () => void {
	const host = document.createElement('div');
	host.setAttribute(APP_HIGHLIGHT_ATTRIBUTE, '');
	host.setAttribute('popover', 'manual');
	host.setAttribute('aria-hidden', 'true');
	host.style.cssText = HOST_STYLE;
	excludeEditorPanel(host);
	for (const element of elements.slice(0, HIGHLIGHT_BOX_LIMIT)) {
		const rect = element.getBoundingClientRect();
		if (rect.width === 0 && rect.height === 0) continue;
		const box = document.createElement('div');
		box.style.cssText = `${BOX_STYLE};left:${rect.left}px;top:${rect.top}px;width:${rect.width}px;height:${rect.height}px`;
		host.appendChild(box);
	}
	document.documentElement.appendChild(host);
	host.showPopover();
	return () => host.remove();
}
