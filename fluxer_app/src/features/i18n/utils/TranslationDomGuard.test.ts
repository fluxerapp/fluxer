// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {commitUnderTranslation, isTranslationDomCrash} from '@app/features/i18n/testing/TranslationCommitHarness';
import {TranslatorPipelines} from '@app/features/i18n/testing/TranslatorSimulation';
import {createElement, type ReactNode} from 'react';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

type GuardModule = typeof import('@app/features/i18n/utils/TranslationDomGuard');
type DeveloperOptionsModule = typeof import('@app/features/devtools/state/DeveloperOptions');

interface LoadedGuard {
	guard: GuardModule;
	developerOptions: DeveloperOptionsModule['default'];
}

let loaded: LoadedGuard | null = null;
let originalInsertBefore: typeof Node.prototype.insertBefore;
let originalRemoveChild: typeof Node.prototype.removeChild;

async function loadGuard(options: {electron?: boolean; disabled?: boolean} = {}): Promise<LoadedGuard> {
	vi.resetModules();
	const {Platform} = await import('@app/features/platform/types/Platform');
	Platform.isElectron = options.electron === true;
	const developerOptions = (await import('@app/features/devtools/state/DeveloperOptions')).default;
	developerOptions.updateOption('disableTranslationDomGuard', options.disabled === true);
	const guard = await import('@app/features/i18n/utils/TranslationDomGuard');
	loaded = {guard, developerOptions};
	return loaded;
}

function translatorDisplacedText(): {container: HTMLElement; text: Text} {
	const container = document.createElement('div');
	container.setAttribute('data-flx', 'guard-fixture');
	const outer = document.createElement('font');
	const inner = document.createElement('font');
	const text = document.createTextNode('Hello');
	inner.appendChild(text);
	outer.appendChild(inner);
	container.appendChild(outer);
	document.body.appendChild(container);
	return {container, text};
}

function detachedTranslatedText(): {container: HTMLElement; text: Text} {
	const container = document.createElement('div');
	const outer = document.createElement('font');
	const inner = document.createElement('font');
	const text = document.createTextNode('Hello');
	inner.appendChild(text);
	outer.appendChild(inner);
	container.appendChild(outer);
	document.body.appendChild(container);
	document.documentElement.classList.add('translated-ltr');
	outer.replaceChildren();
	inner.replaceChildren();
	return {container, text};
}

function unrelatedSubtreeText(): {container: HTMLElement; other: HTMLElement; text: Text} {
	const container = document.createElement('div');
	const other = document.createElement('div');
	other.id = 'other';
	const font = document.createElement('font');
	const text = document.createTextNode('Hello');
	font.appendChild(text);
	other.appendChild(font);
	document.body.append(container, other);
	return {container, other, text};
}

function foreignParentText(): {container: HTMLElement; text: Text} {
	const container = document.createElement('div');
	const elsewhere = document.createElement('div');
	const text = document.createTextNode('Hello');
	elsewhere.appendChild(text);
	document.body.append(container, elsewhere);
	return {container, text};
}

const badge = (on: boolean): ReactNode => (on ? createElement('b', null, '!') : null);
const tail = createElement('i', null, 'x');
const unsafeRow = (on: boolean): ReactNode =>
	createElement('div', null, badge(on), 'Hello', createElement('i', null, 'x'));

beforeEach(() => {
	originalInsertBefore = Node.prototype.insertBefore;
	originalRemoveChild = Node.prototype.removeChild;
});

afterEach(() => {
	loaded?.guard.uninstallTranslationDomGuard();
	loaded?.developerOptions.updateOption('disableTranslationDomGuard', false);
	loaded = null;
	Node.prototype.insertBefore = originalInsertBefore;
	Node.prototype.removeChild = originalRemoveChild;
	document.body.replaceChildren();
	document.documentElement.className = '';
	vi.restoreAllMocks();
});

describe('installation', () => {
	it('patches both prototype methods and restores them by identity on uninstall', async () => {
		const {guard} = await loadGuard();
		guard.installTranslationDomGuard();
		expect(Node.prototype.insertBefore).not.toBe(originalInsertBefore);
		expect(Node.prototype.removeChild).not.toBe(originalRemoveChild);
		expect(guard.getTranslationDomGuardStats().installed).toBe(true);

		guard.installTranslationDomGuard();
		expect(guard.uninstallTranslationDomGuard()).toBe(true);
		expect(Node.prototype.insertBefore).toBe(originalInsertBefore);
		expect(Node.prototype.removeChild).toBe(originalRemoveChild);
		expect(guard.uninstallTranslationDomGuard()).toBe(false);
		expect(guard.getTranslationDomGuardStats().installed).toBe(false);
	});

	it('never installs in Electron', async () => {
		const {guard} = await loadGuard({electron: true});
		guard.installTranslationDomGuard();
		expect(Node.prototype.insertBefore).toBe(originalInsertBefore);
		expect(guard.getTranslationDomGuardStats().installed).toBe(false);
	});

	it('never installs when the Electron native context is unavailable', async () => {
		vi.spyOn(navigator, 'userAgent', 'get').mockReturnValue(
			'Mozilla/5.0 (Macintosh) fluxer/1.0.0 Chrome/140.0.0.0 Electron/38.2.1 Safari/537.36',
		);
		const {guard} = await loadGuard({electron: false});
		guard.installTranslationDomGuard();
		expect(Node.prototype.insertBefore).toBe(originalInsertBefore);
		expect(Node.prototype.removeChild).toBe(originalRemoveChild);
		expect(guard.getTranslationDomGuardStats().installed).toBe(false);
	});

	it('never patches while the kill switch is already set', async () => {
		const {guard} = await loadGuard({disabled: true});
		guard.installTranslationDomGuard();
		expect(Node.prototype.insertBefore).toBe(originalInsertBefore);
		expect(guard.getTranslationDomGuardStats().installed).toBe(false);
	});

	it('uninstalls and reinstalls live when the kill switch is toggled', async () => {
		const {guard, developerOptions} = await loadGuard();
		guard.installTranslationDomGuard();
		expect(guard.getTranslationDomGuardStats().installed).toBe(true);

		developerOptions.updateOption('disableTranslationDomGuard', true);
		expect(Node.prototype.insertBefore).toBe(originalInsertBefore);
		expect(guard.getTranslationDomGuardStats().installed).toBe(false);

		developerOptions.updateOption('disableTranslationDomGuard', false);
		expect(Node.prototype.insertBefore).not.toBe(originalInsertBefore);
		expect(guard.getTranslationDomGuardStats().installed).toBe(true);
	});

	it('exposes stats and a live uninstall on the debug object', async () => {
		const {guard} = await loadGuard();
		guard.installTranslationDomGuard();
		const debug = window.__FLUXER_DEBUG__;
		const getStats = debug?.getTranslationDomGuardStats as
			| (() => ReturnType<GuardModule['getTranslationDomGuardStats']>)
			| undefined;
		expect(getStats?.().installed).toBe(true);
		expect(debug?.uninstallTranslationDomGuard?.()).toBe(true);
		expect(Node.prototype.insertBefore).toBe(originalInsertBefore);
	});
});

describe('reporting', () => {
	it('counts and logs every repaired insertBefore', async () => {
		const {guard} = await loadGuard();
		const consoleError = vi.spyOn(console, 'error').mockImplementation(() => {});
		guard.installTranslationDomGuard();
		const {container, text} = translatorDisplacedText();

		expect(() => container.insertBefore(document.createElement('b'), text)).not.toThrow();

		const stats = guard.getTranslationDomGuardStats();
		expect(stats.insertBefore).toBe(1);
		expect(stats.removeChild).toBe(0);
		expect(stats.declined).toBe(0);
		expect(stats.firstFireAt).not.toBeNull();
		expect(stats.lastPath).toBe('guard-fixture');
		const logged = consoleError.mock.calls.flat().join(' ');
		expect(logged).toContain('TranslationDomGuard');
		expect(logged).toContain('insertBefore');
	});

	it('counts and logs every repaired removeChild', async () => {
		const {guard} = await loadGuard();
		const consoleError = vi.spyOn(console, 'error').mockImplementation(() => {});
		guard.installTranslationDomGuard();
		const {container, text} = translatorDisplacedText();

		expect(() => container.removeChild(text)).not.toThrow();

		const stats = guard.getTranslationDomGuardStats();
		expect(stats.removeChild).toBe(1);
		expect(stats.insertBefore).toBe(0);
		expect(text.parentNode).toBeNull();
		expect(container.innerHTML).toBe('');
		expect(consoleError.mock.calls.flat().join(' ')).toContain('removeChild');
	});

	it('keeps counting after it stops logging', async () => {
		const {guard} = await loadGuard();
		const consoleError = vi.spyOn(console, 'error').mockImplementation(() => {});
		guard.installTranslationDomGuard();
		for (let attempt = 0; attempt < 5; attempt += 1) {
			const {container, text} = translatorDisplacedText();
			container.insertBefore(document.createElement('b'), text);
		}
		expect(guard.getTranslationDomGuardStats().insertBefore).toBe(5);
		expect(consoleError).toHaveBeenCalledTimes(3);
	});

	it('leaves calls on the original path alone', async () => {
		const {guard} = await loadGuard();
		guard.installTranslationDomGuard();
		const parent = document.createElement('div');
		const child = document.createElement('span');
		parent.appendChild(child);
		parent.insertBefore(document.createElement('b'), child);
		parent.removeChild(child);
		expect(parent.innerHTML).toBe('<b></b>');
		const stats = guard.getTranslationDomGuardStats();
		expect(stats.insertBefore).toBe(0);
		expect(stats.removeChild).toBe(0);
		expect(stats.declined).toBe(0);
	});

	it('declines to repair a displacement no translator caused', async () => {
		const {guard} = await loadGuard();
		guard.installTranslationDomGuard();
		const {container, text} = foreignParentText();

		expect(() => container.insertBefore(document.createElement('b'), text)).toThrow(
			/Failed to execute 'insertBefore' on 'Node'/,
		);
		expect(() => container.removeChild(text)).toThrow(/Failed to execute 'removeChild' on 'Node'/);

		const stats = guard.getTranslationDomGuardStats();
		expect(stats.insertBefore).toBe(0);
		expect(stats.removeChild).toBe(0);
		expect(stats.declined).toBe(2);
	});
});

describe('the guard behind the structural fix', () => {
	it('turns the production crash into a reported fire', async () => {
		const {guard} = await loadGuard();
		vi.spyOn(console, 'error').mockImplementation(() => {});
		const unguarded = commitUnderTranslation({
			before: unsafeRow(false),
			after: unsafeRow(true),
			pipeline: TranslatorPipelines.chromeLegacyFont,
		});
		expect(isTranslationDomCrash(unguarded.error)).toBe(true);

		guard.installTranslationDomGuard();
		const guarded = commitUnderTranslation({
			before: unsafeRow(false),
			after: unsafeRow(true),
			pipeline: TranslatorPipelines.chromeLegacyFont,
		});
		expect(guarded.error).toBeNull();
		expect(guard.getTranslationDomGuardStats().insertBefore).toBeGreaterThan(0);
	});

	it('repairs the placement at the position the translator displaced', async () => {
		const {guard} = await loadGuard();
		vi.spyOn(console, 'error').mockImplementation(() => {});
		guard.installTranslationDomGuard();
		let firstTag: string | null = null;
		const result = commitUnderTranslation({
			before: unsafeRow(false),
			after: unsafeRow(true),
			pipeline: TranslatorPipelines.chromeLegacyFont,
			inspect: (host) => {
				firstTag = host.querySelector('div')?.firstElementChild?.tagName ?? null;
			},
		});
		expect(result.error).toBeNull();
		expect(firstTag).toBe('B');
		expect(result.htmlAfterCommit.replace(/<[^>]+>/g, '')).toBe('!~Hello~x');
	});

	it('leaves no empty husk behind after a repaired deletion', async () => {
		const {guard} = await loadGuard();
		vi.spyOn(console, 'error').mockImplementation(() => {});
		guard.installTranslationDomGuard();
		let html = '';
		let fontCount = -1;
		let emptyFontCount = -1;
		const result = commitUnderTranslation({
			before: createElement('div', null, 'Hello', tail),
			after: createElement('div', null, null, tail),
			pipeline: TranslatorPipelines.chromeLegacyFont,
			inspect: (host) => {
				html = host.innerHTML;
				const fonts = Array.from(host.querySelectorAll('font'));
				fontCount = fonts.length;
				emptyFontCount = fonts.filter((font) => font.childNodes.length === 0).length;
			},
		});
		expect(result.error).toBeNull();
		expect(guard.getTranslationDomGuardStats().removeChild).toBeGreaterThan(0);
		expect(html.replace(/<[^>]+>/g, '')).toBe('~x');
		expect(html).not.toContain('Hello');
		expect(emptyFontCount).toBe(0);
		expect(fontCount).toBe(2);
	});

	it('no-ops a removeChild of a node the translator already detached', async () => {
		const {guard} = await loadGuard();
		const consoleError = vi.spyOn(console, 'error').mockImplementation(() => {});
		guard.installTranslationDomGuard();
		const {container, text} = detachedTranslatedText();
		expect(text.parentNode).toBeNull();

		expect(container.removeChild(text)).toBe(text);

		expect(text.parentNode).toBeNull();
		expect(guard.getTranslationDomGuardStats().removeChild).toBe(1);
		expect(consoleError.mock.calls.flat().join(' ')).toContain('removeChild');
	});

	it('does not unwind husks outside the container the removal was addressed to', async () => {
		const {guard} = await loadGuard();
		vi.spyOn(console, 'error').mockImplementation(() => {});
		guard.installTranslationDomGuard();
		const {container, other, text} = unrelatedSubtreeText();

		expect(container.removeChild(text)).toBe(text);

		expect(text.parentNode).toBeNull();
		expect(other.parentNode).toBe(document.body);
		expect(document.getElementById('other')).toBe(other);
		expect(other.innerHTML).toBe('<font></font>');
		expect(guard.getTranslationDomGuardStats().removeChild).toBe(1);
	});
});
