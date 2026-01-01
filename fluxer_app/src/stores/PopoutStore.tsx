/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {makeAutoObservable, runInAction} from 'mobx';
import type {Popout, PopoutKey} from '~/components/uikit/Popout';
import {Logger} from '~/lib/Logger';
import KeyboardModeStore from './KeyboardModeStore';

const logger = new Logger('PopoutStore');

interface FocusRestoreMeta {
	target: HTMLElement | null;
	keyboardModeEnabled: boolean;
}

class PopoutStore {
	popouts: Record<string, Popout> = {};
	private focusReturnMeta = new Map<string, FocusRestoreMeta>();

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	open(popout: Popout): void {
		logger.debug(`Opening popout: ${popout.key || 'unknown'}`);
		const key = this.normalizeKey(popout.key);
		const focusTarget = popout.returnFocusRef?.current ?? popout.target ?? null;
		this.focusReturnMeta.set(key, {
			target: focusTarget,
			keyboardModeEnabled: KeyboardModeStore.keyboardModeEnabled,
		});

		runInAction(() => {
			const normalizedDependsOn = popout.dependsOn != null ? this.normalizeKey(popout.dependsOn) : undefined;
			const popoutWithNormalizedDependency = normalizedDependsOn ? {...popout, dependsOn: normalizedDependsOn} : popout;

			if (!popout.dependsOn) {
				this.popouts = {[key]: popoutWithNormalizedDependency};
			} else {
				const parentChain = this.getParentPopoutChain(normalizedDependsOn!);
				this.popouts = {
					...parentChain,
					[key]: popoutWithNormalizedDependency,
				};
			}
		});

		popout.onOpen?.();
	}

	close(key?: string | number): void {
		logger.debug(`Closing popout${key ? `: ${key}` : ''}`);

		if (key == null) {
			runInAction(() => {
				this.popouts = {};
			});
			this.focusReturnMeta.clear();
			return;
		}

		let closingPopout: Popout | undefined;
		let focusMeta: FocusRestoreMeta | null = null;
		const keyStr = this.normalizeKey(key);

		runInAction(() => {
			const targetPopout = this.popouts[keyStr];
			closingPopout = targetPopout;
			if (!targetPopout) return;

			focusMeta = this.focusReturnMeta.get(keyStr) ?? {
				target: targetPopout.returnFocusRef?.current ?? targetPopout.target ?? null,
				keyboardModeEnabled: KeyboardModeStore.keyboardModeEnabled,
			};

			const newPopouts = {...this.popouts};
			const parentChain = targetPopout.dependsOn
				? this.getParentPopoutChain(this.normalizeKey(targetPopout.dependsOn))
				: {};

			this.removePopoutAndDependents(keyStr, newPopouts);
			Object.assign(newPopouts, parentChain);
			this.popouts = newPopouts;
		});

		closingPopout?.onClose?.();
		this.focusReturnMeta.delete(keyStr);
		this.scheduleFocus(focusMeta);
	}

	closeAll(): void {
		logger.debug('Closing all popouts');
		const currentPopouts = Object.values(this.popouts);
		currentPopouts.forEach((popout) => {
			popout.onClose?.();
		});
		runInAction(() => {
			this.popouts = {};
		});
		this.focusReturnMeta.clear();
	}

	reposition(key: PopoutKey): void {
		const normalizedKey = this.normalizeKey(key);
		const existingPopout = this.popouts[normalizedKey];
		if (!existingPopout) return;

		runInAction(() => {
			this.popouts = {
				...this.popouts,
				[normalizedKey]: {
					...existingPopout,
					shouldReposition: true,
				},
			};
		});
	}

	isOpen(key: PopoutKey): boolean {
		return this.normalizeKey(key) in this.popouts;
	}

	hasDependents(key: PopoutKey): boolean {
		const normalizedKey = this.normalizeKey(key);
		return Object.values(this.popouts).some((popout) =>
			popout.dependsOn ? this.normalizeKey(popout.dependsOn) === normalizedKey : false,
		);
	}

	getPopouts(): Array<Popout> {
		return Object.values(this.popouts);
	}

	private getParentPopoutChain(dependsOnKey: string): Record<string, Popout> {
		const result: Record<string, Popout> = {};
		let currentKey: string | undefined = dependsOnKey;

		while (currentKey != null) {
			const popout: Popout = this.popouts[currentKey];
			if (!popout) break;
			result[currentKey] = popout;
			currentKey = popout.dependsOn ? this.normalizeKey(popout.dependsOn) : undefined;
		}

		return result;
	}

	private removePopoutAndDependents(key: string, popouts: Record<string, Popout>): void {
		const dependentKeys = Object.entries(popouts)
			.filter(([_, popout]) => (popout.dependsOn ? this.normalizeKey(popout.dependsOn) === key : false))
			.map(([k]) => k);

		dependentKeys.forEach((depKey) => {
			this.removePopoutAndDependents(depKey, popouts);
			this.focusReturnMeta.delete(depKey);
		});

		delete popouts[key];
		this.focusReturnMeta.delete(key);
	}

	private scheduleFocus(meta: FocusRestoreMeta | null): void {
		const retries = 5;
		logger.debug(
			`PopoutStore.scheduleFocus target=${meta?.target ? meta.target.tagName : 'null'} keyboardMode=${meta?.keyboardModeEnabled ?? false}`,
		);
		if (!meta || !meta.target) return;
		const {target, keyboardModeEnabled} = meta;
		queueMicrotask(() => {
			const hasHiddenAncestor = (element: HTMLElement): boolean =>
				Boolean(element.closest('[aria-hidden="true"], [data-floating-ui-inert]'));

			const attemptFocus = (remainingRetries: number): void => {
				if (!target.isConnected) {
					logger.debug('PopoutStore.scheduleFocus aborted: target disconnected');
					return;
				}

				if (hasHiddenAncestor(target) && remainingRetries > 0) {
					requestAnimationFrame(() => attemptFocus(remainingRetries - 1));
					return;
				}

				try {
					target.focus({preventScroll: true});
					logger.debug('PopoutStore.scheduleFocus applied focus to target');
				} catch (error) {
					logger.error('PopoutStore.scheduleFocus failed to focus target', error as Error);
					return;
				}

				if (keyboardModeEnabled) {
					logger.debug('PopoutStore.scheduleFocus re-entering keyboard mode');
					KeyboardModeStore.enterKeyboardMode(false);
				}
			};

			attemptFocus(retries);
		});
	}

	private normalizeKey(key: PopoutKey | string): string {
		return typeof key === 'string' ? key : key.toString();
	}
}

export default new PopoutStore();
