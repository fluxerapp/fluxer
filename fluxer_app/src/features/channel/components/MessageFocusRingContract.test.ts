// SPDX-License-Identifier: AGPL-3.0-or-later

import {readFileSync} from 'node:fs';
import {fileURLToPath} from 'node:url';
import {describe, expect, it} from 'vitest';

function readSource(relativePath: string): string {
	return readFileSync(fileURLToPath(new URL(relativePath, import.meta.url)), 'utf8');
}

const messageCss = readSource('../../theme/styles/Message.module.css');
const actionBarCss = readSource('./MessageActionBar.module.css');
const focusRingCss = readSource('../../ui/focus_ring/FocusRing.module.css');
const channelMessageSource = readSource('./ChannelMessage.tsx');
const channelMessagesSource = readSource('./ChannelMessages.tsx');
const messageFocusRing = channelMessageSource.match(/<FocusRing\b[^>]*>/)?.[0] ?? '';

interface CssRule {
	readonly selector: string;
	readonly body: string;
}

function parseCssRules(css: string): Array<CssRule> {
	const withoutComments = css.replace(/\/\*[\s\S]*?\*\//g, '');
	const rules: Array<CssRule> = [];
	for (const match of withoutComments.matchAll(/([^{}]+)\{([^{}]*)\}/g)) {
		rules.push({selector: match[1].replace(/\s+/g, ' ').trim(), body: match[2].trim()});
	}
	return rules;
}

const messageRules = parseCssRules(messageCss);

describe('message focus ring contract', () => {
	it('never draws a ring from a bare :focus selector on a message row', () => {
		expect(messageCss).not.toMatch(/\.message(Compact)?:focus(?!-visible)/);
	});

	it('routes the row ring through the FocusRing framework', () => {
		expect(channelMessageSource).toMatch(/from '@app\/features\/ui\/focus_ring\/FocusRing'/);
		expect(messageFocusRing).not.toBe('');
	});

	it('reads the ring arm from the keyboard navigation rollout', () => {
		expect(channelMessageSource).toMatch(/const keyboardNavigationEnabled = MessageKeyboardFocusRollout\.enabled;/);
	});

	it('only enables the ring in keyboard navigation mode in the experiment arm', () => {
		expect(messageFocusRing).toMatch(/enabled=\{keyboardNavigationEnabled \? keyboardModeEnabled : undefined\}/);
	});

	it('leaves focused descendants their own rings instead of drawing the row ring over them', () => {
		expect(messageFocusRing).not.toMatch(/\bwithin\b/);
	});

	it('scopes message list rings inside the scroll content in the experiment arm', () => {
		const scopedList = channelMessagesSource.match(
			/\{keyboardNavigationEnabled \? \(\s*<FocusRingScope containerRef=\{scrollerInnerRef\}[^>]*>\s*\{messageListContent\}\s*<\/FocusRingScope>\s*\) : \(\s*messageListContent\s*\)\}/,
		);
		expect(scopedList).not.toBeNull();
		expect(channelMessagesSource).toMatch(/const keyboardNavigationEnabled = MessageKeyboardFocusRollout\.enabled;/);
	});

	it('insets the ring inside the row in the experiment arm and keeps the default geometry in control', () => {
		expect(focusRingCss).toMatch(/pointer-events:\s*none/);
		expect(messageFocusRing).toMatch(/offset=\{keyboardNavigationEnabled \? -2 : undefined\}/);
	});

	it('stacks the ring below the action bar', () => {
		expect(actionBarCss).toMatch(/z-index:\s*var\(--z-index-elevated-1\)/);
		expect(messageFocusRing).not.toMatch(/zIndex=/);
	});

	it('measures the ring from an anchor inset by the row bleed in the experiment arm', () => {
		expect(messageFocusRing).toMatch(/ringTarget=\{keyboardNavigationEnabled \? focusRingAnchorRef : undefined\}/);
		expect(channelMessageSource).toMatch(/className=\{styles\.focusRingAnchor\}/);
		const anchor = messageRules.find((rule) => rule.selector === '.focusRingAnchor');
		expect(anchor).toBeDefined();
		expect(anchor?.body).toMatch(/position:\s*absolute/);
		expect(anchor?.body).toMatch(/inset-inline:\s*var\(--message-inline-bleed/);
		expect(anchor?.body).toMatch(/pointer-events:\s*none/);
	});

	it('derives every message row bleed from the shared bleed variable', () => {
		const bleedingRows = messageRules.filter(
			(rule) => /^\.message(Compact|Preview)?$/.test(rule.selector) && /margin-inline\s*:/.test(rule.body),
		);
		expect(bleedingRows.length).toBeGreaterThan(0);
		for (const rule of bleedingRows) {
			const margin = rule.body.match(/margin-inline:\s*([^;]+)/)?.[1]?.trim();
			expect(margin).toBeDefined();
			expect(margin === '0' || margin === 'calc(-1 * var(--message-inline-bleed))').toBe(true);
		}
	});

	it('falls back to a system outline under forced colors', () => {
		const forcedColors = focusRingCss.match(/@media \(forced-colors: active\) \{\n\t\.focusRing \{([^{}]*)\}/)?.[1];
		expect(forcedColors).toBeDefined();
		expect(forcedColors).toMatch(/box-shadow:\s*none/);
		expect(forcedColors).toMatch(/outline-color:\s*Highlight/);
	});
});
