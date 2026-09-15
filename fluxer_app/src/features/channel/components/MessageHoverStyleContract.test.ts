// SPDX-License-Identifier: AGPL-3.0-or-later

import {readFileSync} from 'node:fs';
import {fileURLToPath} from 'node:url';
import {describe, expect, it} from 'vitest';

interface CssRule {
	readonly selector: string;
	readonly body: string;
}

function readSource(relativePath: string): string {
	return readFileSync(fileURLToPath(new URL(relativePath, import.meta.url)), 'utf8');
}

function parseCssRules(css: string): Array<CssRule> {
	const withoutComments = css.replace(/\/\*[\s\S]*?\*\//g, '');
	const rules: Array<CssRule> = [];
	for (const match of withoutComments.matchAll(/([^{}]+)\{([^{}]*)\}/g)) {
		rules.push({selector: match[1].replace(/\s+/g, ''), body: match[2].trim()});
	}
	return rules;
}

const actionBarCss = readSource('./MessageActionBar.module.css');
const messageCss = readSource('../../theme/styles/Message.module.css');
const channelMessageSource = readSource('./ChannelMessage.tsx');
const hoverStateSource = readSource('./MessageHoverState.ts');

const actionBarContainerRules = parseCssRules(actionBarCss).filter((rule) =>
	rule.selector.includes('.actionBarContainer'),
);
const revealingRules = actionBarContainerRules.filter((rule) => /visibility:\s*visible/.test(rule.body));

describe('message hover style contract', () => {
	it('reveals the action bar from the tracked hover state', () => {
		const hoverRevealRules = revealingRules.filter((rule) => !rule.selector.includes(':focus-visible'));
		expect(hoverRevealRules.length).toBeGreaterThan(0);
		for (const rule of hoverRevealRules) {
			expect(rule.selector).not.toMatch(/:hover/);
			expect(rule.selector).toMatch(/data-flx-action-bar-active/);
		}
	});

	it('paints the row highlight from the class React drives', () => {
		const highlightRules = parseCssRules(messageCss).filter(
			(rule) =>
				rule.selector.includes('.messageHovered') &&
				!rule.selector.includes(':hover') &&
				/(background-color|opacity):/.test(rule.body),
		);
		expect(highlightRules.length).toBeGreaterThan(0);
	});

	it('derives the row highlight and the action bar from the same hover state', () => {
		const highlightSource = channelMessageSource.match(/(\w+) && !isPreview && styles\.messageHovered/)?.[1];
		const actionBarSourceState = channelMessageSource.match(/const isActionBarActive = (\w+) \|\|/)?.[1];
		expect(highlightSource).toBeDefined();
		expect(actionBarSourceState).toBe(highlightSource);
		expect(channelMessageSource).toMatch(
			/data-flx-action-bar-active=\{shouldShowActionBar && isActionBarActive \? 'true' : undefined\}/,
		);
	});

	it('resolves the tracked hover state without the :hover chain', () => {
		expect(readSource('./MessageHoverTracking.ts')).not.toMatch(/matches\(':hover'\)/);
		expect(hoverStateSource).not.toMatch(/matches\(':hover'\)/);
		expect(hoverStateSource).toMatch(/registerMessageHoverTarget\(element, setDesktopHoverState\)/);
	});
});
