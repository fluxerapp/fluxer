// SPDX-License-Identifier: AGPL-3.0-or-later

import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import {Platform} from '@app/features/platform/types/Platform';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {getFluxerDebugObject} from '@app/features/platform/utils/FluxerDebugGlobal';
import {hasUnavailableElectronNativeContext} from '@app/features/ui/utils/NativeUtils';
import {reaction} from 'mobx';

const logger = new Logger('TranslationDomGuard');
const MAX_LOGGED_FIRES_PER_KIND = 3;
const TRANSLATED_ROOT_CLASS_PATTERN = /(^|\s)translated-(ltr|rtl)(\s|$)/;
const MICROSOFT_TRANSLATOR_ATTRIBUTES = ['_msttexthash', '_msthash'];

type GuardedMethod = 'insertBefore' | 'removeChild';

export interface TranslationDomGuardStats {
	installed: boolean;
	insertBefore: number;
	removeChild: number;
	declined: number;
	firstFireAt: number | null;
	lastPath: string | null;
}

const stats: TranslationDomGuardStats = {
	installed: false,
	insertBefore: 0,
	removeChild: 0,
	declined: 0,
	firstFireAt: null,
	lastPath: null,
};
const loggedByKind: Record<GuardedMethod, number> = {insertBefore: 0, removeChild: 0};
let restorePrototypes: (() => void) | null = null;
let stopWatchingKillSwitch: (() => void) | null = null;

function describe(node: Node | null): string | null {
	const element = node instanceof Element ? node : (node?.parentElement ?? null);
	if (!element) {
		return null;
	}
	return element.closest('[data-flx]')?.getAttribute('data-flx') ?? element.tagName.toLowerCase();
}

function isTranslatorDisplacement(subject: Node, container: Node): boolean {
	let node: Node | null = subject;
	while (node != null && node !== container) {
		if (node instanceof Element) {
			const element = node;
			if (element.tagName === 'FONT') {
				return true;
			}
			if (MICROSOFT_TRANSLATOR_ATTRIBUTES.some((attribute) => element.hasAttribute(attribute))) {
				return true;
			}
		}
		node = node.parentNode;
	}
	const root = subject.ownerDocument?.documentElement;
	return root != null && TRANSLATED_ROOT_CLASS_PATTERN.test(root.className);
}

function recordFire(kind: GuardedMethod, container: Node, subject: Node): void {
	stats[kind] += 1;
	stats.firstFireAt ??= Date.now();
	const path = describe(container) ?? describe(subject) ?? 'unknown';
	stats.lastPath = path;
	if (loggedByKind[kind] < MAX_LOGGED_FIRES_PER_KIND) {
		loggedByKind[kind] += 1;
		logger.error(
			`Repaired a ${kind} against a node a page translator re-parented at ${path}. A translation-unsafe text node is still rendered there; fix the component instead of relying on this guard.`,
		);
	}
}

function patchPrototypes(): void {
	if (restorePrototypes != null) {
		return;
	}
	const originalInsertBefore = Node.prototype.insertBefore;
	const originalRemoveChild = Node.prototype.removeChild;
	const guardedInsertBefore = function insertBefore<T extends Node>(this: Node, node: T, child: Node | null): T {
		if (child == null || child.parentNode === this) {
			return originalInsertBefore.call(this, node, child) as T;
		}
		if (!isTranslatorDisplacement(child, this)) {
			stats.declined += 1;
			return originalInsertBefore.call(this, node, child) as T;
		}
		recordFire('insertBefore', this, child);
		let anchor: Node | null = child;
		while (anchor != null && anchor.parentNode !== this) {
			anchor = anchor.parentNode;
		}
		return originalInsertBefore.call(this, node, anchor) as T;
	};
	const guardedRemoveChild = function removeChild<T extends Node>(this: Node, child: T): T {
		const actualParent = child.parentNode;
		if (actualParent === this) {
			return originalRemoveChild.call(this, child) as T;
		}
		if (!isTranslatorDisplacement(child, this)) {
			stats.declined += 1;
			return originalRemoveChild.call(this, child) as T;
		}
		recordFire('removeChild', this, child);
		if (actualParent == null) {
			return child;
		}
		const removed = originalRemoveChild.call(actualParent, child) as T;
		if (!(this instanceof Node) || !this.contains(actualParent)) {
			return removed;
		}
		let husk: Node | null = actualParent;
		while (husk != null && husk !== this && husk.childNodes.length === 0) {
			const parent: Node | null = husk.parentNode;
			if (parent == null) {
				break;
			}
			originalRemoveChild.call(parent, husk);
			husk = parent;
		}
		return removed;
	};
	Node.prototype.insertBefore = guardedInsertBefore;
	Node.prototype.removeChild = guardedRemoveChild;
	restorePrototypes = () => {
		if (Node.prototype.insertBefore !== guardedInsertBefore || Node.prototype.removeChild !== guardedRemoveChild) {
			logger.warn('Node prototype methods were replaced after install; restoring the originals anyway.');
		}
		Node.prototype.insertBefore = originalInsertBefore;
		Node.prototype.removeChild = originalRemoveChild;
	};
	stats.installed = true;
}

function unpatchPrototypes(): void {
	if (restorePrototypes == null) {
		return;
	}
	restorePrototypes();
	restorePrototypes = null;
	stats.installed = false;
}

export function getTranslationDomGuardStats(): TranslationDomGuardStats {
	return {...stats};
}

export function uninstallTranslationDomGuard(): boolean {
	const wasInstalled = restorePrototypes != null;
	stopWatchingKillSwitch?.();
	stopWatchingKillSwitch = null;
	unpatchPrototypes();
	return wasInstalled;
}

export function installTranslationDomGuard(): void {
	if (stopWatchingKillSwitch != null || restorePrototypes != null) {
		return;
	}
	if (typeof Node === 'undefined' || Platform.isElectron || hasUnavailableElectronNativeContext()) {
		return;
	}
	stopWatchingKillSwitch = reaction(
		() => DeveloperOptions.disableTranslationDomGuard,
		(disabled) => {
			if (disabled) {
				unpatchPrototypes();
				logger.info('Translation DOM guard disabled by developer option.');
			} else {
				patchPrototypes();
			}
		},
		{fireImmediately: true},
	);
	const debug = getFluxerDebugObject();
	if (debug) {
		debug.getTranslationDomGuardStats = getTranslationDomGuardStats;
		debug.uninstallTranslationDomGuard = uninstallTranslationDomGuard;
	}
}
