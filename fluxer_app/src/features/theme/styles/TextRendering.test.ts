// SPDX-License-Identifier: AGPL-3.0-or-later

import {readdirSync, readFileSync} from 'node:fs';
import {join} from 'node:path';
import {fileURLToPath} from 'node:url';
import {describe, expect, it} from 'vitest';

const APP_SRC = fileURLToPath(new URL('../../../', import.meta.url));

const FORBIDDEN_SMOOTHING_PROPERTIES = ['-webkit-font-smoothing', 'font-smooth', 'text-rendering'];
const LAYER_PROMOTING_WILL_CHANGE = /transform|opacity|filter/;
const WILL_CHANGE = /will-change\s*:\s*([^;]*)/;

const ANIMATION_STATE_SELECTOR = /\[data-(dragging|state|animating)|:hover|:active|Animating|Animated|Dragging/;

const NON_TEXT_WILL_CHANGE_ALLOWLIST = new Set([
	'features/channel/components/GifPicker.module.css .loadingSkeletonBlob',
	'features/channel/components/GuildMembersPage.module.css .progressBar',
	'features/channel/components/direct_message/DMChannelView.module.css .callParticipantAvatar',
	'features/messaging/components/modals/MediaModal.module.css .desktopViewerContent',
	'features/ui/components/form/FormSwitch.module.css .switchThumb',
	'features/ui/scroller/ScrollerTrack.module.css .thumb',
	'features/user/components/profile/UserProfileLoadingSkeleton.module.css .pulse',
	'features/voice/components/LiveRecordingWaveform.module.css .track',
	'features/voice/components/PiPOverlay.module.css .container',
]);

function collectFiles(directory: string, extension: string, out: Array<string> = []): Array<string> {
	for (const entry of readdirSync(directory, {withFileTypes: true})) {
		const full = join(directory, entry.name);
		if (entry.isDirectory()) {
			collectFiles(full, extension, out);
		} else if (entry.name.endsWith(extension)) {
			out.push(full);
		}
	}
	return out;
}

function rules(css: string): Array<{selector: string; declarations: string}> {
	const source = css.replace(/\/\*[\s\S]*?\*\//g, '');
	const out: Array<{selector: string; declarations: string}> = [];
	const chain: Array<string> = [];
	let buffer = '';
	for (const char of source) {
		if (char === '{') {
			chain.push(buffer.split(/[;}]/).pop()?.trim().replace(/\s+/g, ' ') ?? '');
			buffer = '';
		} else if (char === '}') {
			if (chain.length > 0) {
				out.push({selector: chain.join(' '), declarations: buffer});
				chain.pop();
			}
			buffer = '';
		} else {
			buffer += char;
		}
	}
	return out;
}

function relative(file: string): string {
	return file.slice(APP_SRC.length);
}

describe('text rendering', () => {
	const stylesheets = collectFiles(APP_SRC, '.css');

	it('finds the authored stylesheets it is meant to guard', () => {
		expect(stylesheets.length).toBeGreaterThan(100);
	});

	it('never overrides font smoothing, which would disable ClearType on Windows', () => {
		const offenders: Array<string> = [];
		for (const file of stylesheets) {
			const css = readFileSync(file, 'utf8');
			for (const property of FORBIDDEN_SMOOTHING_PROPERTIES) {
				if (css.includes(property)) {
					offenders.push(`${relative(file)}: ${property}`);
				}
			}
		}
		expect(offenders).toEqual([]);
	});

	it('only allows a layer-promoting will-change when it is animation-scoped or a reviewed non-text rule', () => {
		const offenders: Array<string> = [];
		for (const file of stylesheets) {
			for (const {selector, declarations} of rules(readFileSync(file, 'utf8'))) {
				const willChange = WILL_CHANGE.exec(declarations);
				if (willChange == null) continue;
				if (!LAYER_PROMOTING_WILL_CHANGE.test(willChange[1]!)) continue;
				if (ANIMATION_STATE_SELECTOR.test(selector)) continue;
				if (NON_TEXT_WILL_CHANGE_ALLOWLIST.has(`${relative(file)} ${selector}`)) continue;
				offenders.push(`${relative(file)} ${selector}: will-change:${willChange[1]!.trim()}`);
			}
		}
		expect(offenders).toEqual([]);
	});

	it('never promotes a layer with an inline willChange style, which the stylesheet guard cannot see', () => {
		const offenders: Array<string> = [];
		for (const file of collectFiles(APP_SRC, '.tsx')) {
			if (readFileSync(file, 'utf8').includes('willChange')) {
				offenders.push(relative(file));
			}
		}
		expect(offenders).toEqual([]);
	});
});

describe('popout text rendering', () => {
	it('shadows popouts with box-shadow, since any filter disables subpixel antialiasing', () => {
		const css = readFileSync(join(APP_SRC, 'features/ui/popover/PopoverPopout.module.css'), 'utf8');
		expect(css).toContain('box-shadow: 0 0.5rem 1rem var(--popout-shadow);');
		expect(css).not.toMatch(/(^|[\s;])filter\s*:/m);
		expect(css).not.toContain('popoutStableText');
	});
});

const FRACTIONAL_OPACITY = /(^|[;\s])opacity\s*:\s*(0?\.\d+)\s*(;|$)/;
const TEXT_DECLARATION =
	/(^|[;\s])(color|font-size|font-weight|font-family|font-style|line-height|letter-spacing|text-transform|text-overflow|white-space)\s*:/;
const TRANSIENT_STATE_SELECTOR =
	/:hover|:focus|:active|:disabled|::before|::after|\[data-|%|\bfrom\b|\bto\b|Animating|Animated|Dragging|Hover|Pressed|Selected/;

const PERSISTENT_TEXT_OPACITY_ALLOWLIST = new Set([
	'features/channel/components/ChannelDivider.module.css .unreadBadge',
	'features/theme/styles/AttachmentFile.module.css .statusBadge',
	'features/app/components/layout/sidebar_nav/GuildListDMItem.module.css .muted',
	'features/app/components/layout/NativeTitlebar.module.css .left',
	'features/app/components/layout/ChannelItem.module.css .channelItemMutedState',
	'features/app/components/layout/FavoritesChannelListContent.module.css .favoriteItemMuted',
	'features/channel/components/direct_message/DirectMessageList.module.css .dmItemMuted',
	'features/channel/components/direct_message/DirectMessageList.module.css .dmItemMobileMuted',
	'features/ui/action_menu/ContextMenu.module.css .item.disabled',
	'features/ui/action_menu/MenuItem.module.css .sliderItem.disabled',
	'features/app/components/dialogs/Modal.module.css .headerInner button',
	'features/channel/components/active_now/ActiveNowSidebar.module.css .emptyIcon',
	'features/channel/components/shared/MemberListUnavailableFallback.module.css .icon',
	'features/messaging/components/markdown/renderers/MessageJumpLink.module.css .divider',
	'features/search/components/search/SearchFilterChip.module.css .removeButton',
	'features/theme/styles/AttachmentGridItem.module.css .audioPlaceholder svg',
	'features/theme/styles/AttachmentMosaic.module.css .audioPlaceholder svg',
	'features/user/components/modals/UserProfileModal.module.css .emptyStateIcon',
]);

describe('persistent text opacity', () => {
	it('dims text through colour, not through a layer opacity that would kill subpixel antialiasing', () => {
		const offenders: Array<string> = [];
		for (const file of collectFiles(APP_SRC, '.module.css')) {
			for (const {selector, declarations} of rules(readFileSync(file, 'utf8'))) {
				if (!FRACTIONAL_OPACITY.test(declarations)) continue;
				if (!TEXT_DECLARATION.test(declarations)) continue;
				if (TRANSIENT_STATE_SELECTOR.test(selector)) continue;
				const key = `${relative(file)} ${selector}`;
				if (PERSISTENT_TEXT_OPACITY_ALLOWLIST.has(key)) continue;
				offenders.push(key);
			}
		}
		expect(offenders).toEqual([]);
	});
});
