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

const EXPERIMENT_CLASS = 'experiment-message-hover-tracking';
const TREATMENT_GATE = `html.${EXPERIMENT_CLASS}`;
const CONTROL_GATE = `:not(.${EXPERIMENT_CLASS})`;

const actionBarCss = readSource('./MessageActionBar.module.css');
const messageCss = readSource('../../theme/styles/Message.module.css');
const actionBarSource = readSource('./MessageActionBar.tsx');
const channelMessageSource = readSource('./ChannelMessage.tsx');
const hoverStateSource = readSource('./MessageHoverState.ts');
const rolloutSource = readSource('../state/MessageHoverTrackingRollout.ts');
const appSource = readSource('../../../app/App.tsx');

const actionBarContainerRules = parseCssRules(actionBarCss).filter((rule) =>
	rule.selector.includes('.actionBarContainer'),
);
const revealingRules = actionBarContainerRules.filter((rule) => /visibility:\s*visible/.test(rule.body));
const treatmentRevealRules = revealingRules.filter((rule) => rule.selector.includes(TREATMENT_GATE));
const controlRevealRules = revealingRules.filter((rule) => rule.selector.includes(CONTROL_GATE));

describe('message hover style contract', () => {
	it('reveals the action bar from the tracked hover state in the experiment arm', () => {
		expect(treatmentRevealRules.length).toBeGreaterThan(0);
		for (const rule of treatmentRevealRules) {
			expect(rule.selector).not.toMatch(/:hover/);
			expect(rule.selector).toMatch(/data-flx-action-bar-active/);
		}
	});

	it('keeps the legacy :hover reveal behind the control arm', () => {
		expect(controlRevealRules.length).toBeGreaterThan(0);
		expect(controlRevealRules.some((rule) => rule.selector.includes(':hover'))).toBe(true);
		for (const rule of controlRevealRules) {
			expect(rule.selector).not.toMatch(/data-flx-action-bar-active/);
		}
	});

	it('gives every action bar reveal rule exactly one arm', () => {
		for (const rule of revealingRules) {
			if (rule.selector.includes(':focus-visible')) continue;
			const treatment = rule.selector.includes(TREATMENT_GATE);
			const control = rule.selector.includes(CONTROL_GATE);
			expect(treatment || control).toBe(true);
			expect(treatment && control).toBe(false);
		}
	});

	it('never reveals the action bar from the message stylesheet in the experiment arm', () => {
		const buttonRevealRules = parseCssRules(messageCss).filter(
			(rule) => /\.buttons\b/.test(rule.selector) && /opacity:\s*1/.test(rule.body),
		);
		expect(buttonRevealRules.length).toBeGreaterThan(0);
		for (const rule of buttonRevealRules) {
			expect(rule.selector).toContain(CONTROL_GATE);
		}
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

	it('resolves the tracked hover state without the retained :hover chain', () => {
		expect(readSource('./MessageHoverTracking.ts')).not.toMatch(/matches\(':hover'\)/);
		const legacyOracle = hoverStateSource.match(/const isPointerInsideMessage[\s\S]*?\n\t\};/)?.[0];
		expect(legacyOracle).toBeDefined();
		expect(legacyOracle).toMatch(/matches\(':hover'\)/);
		expect(hoverStateSource).toMatch(/if \(!trackingEnabled \|\| mobileLayoutEnabled\) \{/);
		expect(hoverStateSource).toMatch(
			/if \(trackingEnabled \|\| mobileLayoutEnabled \|\| !messageRef\.current\) return;/,
		);
	});

	it('drives the stylesheet arm from the rollout assignment', () => {
		expect(rolloutSource).toContain(`'${EXPERIMENT_CLASS}'`);
		expect(actionBarCss).toContain(EXPERIMENT_CLASS);
		expect(messageCss).toContain(EXPERIMENT_CLASS);
		expect(appSource).toMatch(
			/useDocumentClassToggle\(MESSAGE_HOVER_TRACKING_EXPERIMENT_CLASS, MessageHoverTrackingRollout\.enabled\)/,
		);
	});

	it('keeps the control arm reachable from the action bar component', () => {
		expect(actionBarSource).toMatch(/messageStyles\.buttons/);
		expect(actionBarSource).toMatch(/styles\.actionBarPinned/);
	});
});
