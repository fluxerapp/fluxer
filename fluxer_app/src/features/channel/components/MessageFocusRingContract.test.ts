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
const messageFocusRing = channelMessageSource.match(/<FocusRing\b[^>]*>/)?.[0] ?? '';

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
		expect(messageFocusRing).toMatch(/within=\{keyboardNavigationEnabled\}/);
	});

	it('insets the ring inside the row in the experiment arm and keeps the default geometry in control', () => {
		expect(focusRingCss).toMatch(/pointer-events:\s*none/);
		expect(messageFocusRing).toMatch(/offset=\{keyboardNavigationEnabled \? -2 : undefined\}/);
	});

	it('stacks the ring below the action bar', () => {
		expect(actionBarCss).toMatch(/z-index:\s*var\(--z-index-elevated-1\)/);
	});

	it('falls back to a system outline under forced colors', () => {
		const forcedColors = focusRingCss.match(/@media \(forced-colors: active\) \{\n\t\.focusRing \{([^{}]*)\}/)?.[1];
		expect(forcedColors).toBeDefined();
		expect(forcedColors).toMatch(/box-shadow:\s*none/);
		expect(forcedColors).toMatch(/outline-color:\s*Highlight/);
	});
});
