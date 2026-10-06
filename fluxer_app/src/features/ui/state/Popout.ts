// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import type {Popout as PopoutDefinition, PopoutKey} from '@app/features/ui/popover';
import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import {shouldRestoreFocusToTarget} from '@app/features/ui/utils/PointerActivationFocus';
import {makeAutoObservable, runInAction} from 'mobx';

const logger = new Logger('Popout');

interface FocusRestoreMeta {
	target: HTMLElement | null;
	keyboardModeEnabled: boolean;
	restoreFocus: boolean;
}

class Popout {
	popouts: Record<string, PopoutDefinition> = {};
	popoutOrder: Array<string> = [];
	closingPopouts: Record<string, true> = {};
	private focusReturnMeta = new Map<string, FocusRestoreMeta>();

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	open(popout: PopoutDefinition): void {
		logger.debug(`Opening popout: ${popout.key || 'unknown'}`);
		const key = this.normalizeKey(popout.key);
		const ownerDocument = this.getOwnerDocument(popout);
		const focusTarget = popout.returnFocusRef?.current ?? popout.target ?? null;
		const keyboardModeEnabled = KeyboardMode.keyboardModeEnabled;
		const restoreFocus = shouldRestoreFocusToTarget(focusTarget, keyboardModeEnabled);
		this.focusReturnMeta.set(key, {
			target: restoreFocus ? focusTarget : null,
			keyboardModeEnabled,
			restoreFocus,
		});
		runInAction(() => {
			const normalizedDependsOn = popout.dependsOn != null ? this.normalizeKey(popout.dependsOn) : undefined;
			const popoutWithFocusPolicy = {...popout, returnFocusOnClose: restoreFocus};
			const popoutWithNormalizedDependency = normalizedDependsOn
				? {...popoutWithFocusPolicy, dependsOn: normalizedDependsOn}
				: popoutWithFocusPolicy;
			if (!popout.dependsOn) {
				const nextPopouts = {...this.popouts};
				this.removePopoutsForDocument(ownerDocument, nextPopouts, new Set([key]));
				nextPopouts[key] = popoutWithNormalizedDependency;
				this.popouts = nextPopouts;
				this.popoutOrder = [...this.popoutOrder.filter((k) => k in nextPopouts && k !== key), key];
				this.closingPopouts = this.filterClosingPopouts(nextPopouts, key);
			} else {
				const parentChain = this.getParentPopoutChain(normalizedDependsOn!);
				const preservedKeys = new Set([...Object.keys(parentChain), key]);
				const nextPopouts = {
					...this.popouts,
					[key]: popoutWithNormalizedDependency,
				};
				this.removePopoutsForDocument(ownerDocument, nextPopouts, preservedKeys);
				Object.assign(nextPopouts, parentChain);
				this.popouts = nextPopouts;
				this.popoutOrder = [...this.popoutOrder.filter((k) => k in nextPopouts && k !== key), key];
				this.closingPopouts = this.filterClosingPopouts(nextPopouts, key);
			}
		});
		popout.onOpen?.();
	}

	requestClose(key?: string | number): void {
		if (key == null) {
			this.requestCloseAll();
			return;
		}
		const keyStr = this.normalizeKey(key);
		const popout = this.popouts[keyStr];
		if (!popout) return;
		if (!this.shouldAnimateClose(popout)) {
			this.close(keyStr);
			return;
		}
		if (this.closingPopouts[keyStr]) return;
		runInAction(() => {
			this.closingPopouts = {...this.closingPopouts, [keyStr]: true};
		});
	}

	close(key?: string | number): void {
		logger.debug(`Closing popout${key ? `: ${key}` : ''}`);
		if (key == null) {
			runInAction(() => {
				this.popouts = {};
				this.popoutOrder = [];
				this.closingPopouts = {};
			});
			this.focusReturnMeta.clear();
			return;
		}
		let closingPopout: PopoutDefinition | undefined;
		let focusMeta: FocusRestoreMeta | null = null;
		const keyStr = this.normalizeKey(key);
		runInAction(() => {
			const targetPopout = this.popouts[keyStr];
			closingPopout = targetPopout;
			if (!targetPopout) return;
			const focusTarget = targetPopout.returnFocusRef?.current ?? targetPopout.target ?? null;
			const keyboardModeEnabled = KeyboardMode.keyboardModeEnabled;
			const restoreFocus = shouldRestoreFocusToTarget(focusTarget, keyboardModeEnabled);
			focusMeta = this.focusReturnMeta.get(keyStr) ?? {
				target: restoreFocus ? focusTarget : null,
				keyboardModeEnabled,
				restoreFocus,
			};
			const newPopouts = {...this.popouts};
			const parentChain = targetPopout.dependsOn
				? this.getParentPopoutChain(this.normalizeKey(targetPopout.dependsOn))
				: {};
			this.removePopoutAndDependents(keyStr, newPopouts);
			Object.assign(newPopouts, parentChain);
			this.popouts = newPopouts;
			this.popoutOrder = this.popoutOrder.filter((k) => k in newPopouts);
			const nextClosingPopouts = {...this.closingPopouts};
			delete nextClosingPopouts[keyStr];
			this.closingPopouts = nextClosingPopouts;
		});
		closingPopout?.onClose?.();
		this.focusReturnMeta.delete(keyStr);
		this.scheduleFocus(focusMeta);
	}

	closeAll(): void {
		logger.debug('Closing all popouts');
		this.requestCloseAll();
	}

	closeAllForDocument(ownerDocument: Document): void {
		logger.debug('Closing all popouts for document');
		this.requestCloseAll(ownerDocument);
	}

	closeAllImmediately(ownerDocument?: Document): void {
		const currentPopouts = Object.values(this.popouts).filter(
			(popout) => !ownerDocument || this.getOwnerDocument(popout) === ownerDocument,
		);
		currentPopouts.forEach((popout) => {
			popout.onClose?.();
		});
		if (ownerDocument) {
			runInAction(() => {
				const nextPopouts = {...this.popouts};
				this.removePopoutsForDocument(ownerDocument, nextPopouts);
				this.popouts = nextPopouts;
				this.popoutOrder = this.popoutOrder.filter((key) => key in nextPopouts);
				this.closingPopouts = this.filterClosingPopouts(nextPopouts);
			});
			return;
		}
		runInAction(() => {
			this.popouts = {};
			this.popoutOrder = [];
			this.closingPopouts = {};
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

	shouldReturnFocus(key: PopoutKey): boolean {
		return this.focusReturnMeta.get(this.normalizeKey(key))?.restoreFocus === true;
	}

	isClosing(key: PopoutKey): boolean {
		return this.closingPopouts[this.normalizeKey(key)] === true;
	}

	hasDependents(key: PopoutKey): boolean {
		const normalizedKey = this.normalizeKey(key);
		return Object.values(this.popouts).some((popout) =>
			popout.dependsOn ? this.normalizeKey(popout.dependsOn) === normalizedKey : false,
		);
	}

	isDependentOn(key: PopoutKey, ancestorKey: PopoutKey): boolean {
		const normalizedAncestorKey = this.normalizeKey(ancestorKey);
		let currentKey: string | undefined = this.normalizeKey(key);
		while (currentKey != null) {
			const popout: PopoutDefinition | undefined = this.popouts[currentKey];
			const dependsOn = popout?.dependsOn;
			if (dependsOn == null) return false;
			const normalizedDependsOn = this.normalizeKey(dependsOn);
			if (normalizedDependsOn === normalizedAncestorKey) return true;
			currentKey = normalizedDependsOn;
		}
		return false;
	}

	getPopouts(ownerDocument: Document = document): Array<PopoutDefinition> {
		return this.popoutOrder
			.map((key) => this.popouts[key])
			.filter(
				(popout): popout is PopoutDefinition => popout != null && this.getOwnerDocument(popout) === ownerDocument,
			);
	}

	private getParentPopoutChain(dependsOnKey: string): Record<string, PopoutDefinition> {
		const result: Record<string, PopoutDefinition> = {};
		let currentKey: string | undefined = dependsOnKey;
		while (currentKey != null) {
			const popout: PopoutDefinition = this.popouts[currentKey];
			if (!popout) break;
			result[currentKey] = popout;
			currentKey = popout.dependsOn ? this.normalizeKey(popout.dependsOn) : undefined;
		}
		return result;
	}

	private removePopoutAndDependents(key: string, popouts: Record<string, PopoutDefinition>): void {
		const dependentKeys = Object.entries(popouts)
			.filter(([_, popout]) => (popout.dependsOn ? this.normalizeKey(popout.dependsOn) === key : false))
			.map(([k]) => k);
		dependentKeys.forEach((depKey) => {
			this.removePopoutAndDependents(depKey, popouts);
			this.focusReturnMeta.delete(depKey);
		});
		delete popouts[key];
		this.focusReturnMeta.delete(key);
		delete this.closingPopouts[key];
	}

	private requestCloseAll(ownerDocument?: Document): void {
		const animatedKeys = Object.entries(this.popouts)
			.filter(([_, popout]) => !ownerDocument || this.getOwnerDocument(popout) === ownerDocument)
			.filter(([_, popout]) => this.shouldAnimateClose(popout))
			.map(([key]) => key);
		const scopedKeys = Object.entries(this.popouts)
			.filter(([_, popout]) => !ownerDocument || this.getOwnerDocument(popout) === ownerDocument)
			.map(([key]) => key);
		if (scopedKeys.length === 0) {
			return;
		}
		if (animatedKeys.length === 0) {
			this.closeAllImmediately(ownerDocument);
			return;
		}
		runInAction(() => {
			this.closingPopouts = animatedKeys.reduce<Record<string, true>>(
				(result, key) => {
					result[key] = true;
					return result;
				},
				{...this.closingPopouts},
			);
		});
		scopedKeys.forEach((key) => {
			if (!animatedKeys.includes(key)) {
				this.close(key);
			}
		});
	}

	private shouldAnimateClose(popout: PopoutDefinition): boolean {
		return popout.animationType === 'profile-slide' || popout.animationType === 'profile-slide-inverted';
	}

	private getOwnerDocument(popout: PopoutDefinition): Document {
		return popout.target.ownerDocument ?? document;
	}

	private removePopoutsForDocument(
		ownerDocument: Document,
		popouts: Record<string, PopoutDefinition>,
		preservedKeys: Set<string> = new Set(),
	): void {
		for (const [key, popout] of Object.entries(popouts)) {
			if (preservedKeys.has(key)) continue;
			if (this.getOwnerDocument(popout) !== ownerDocument) continue;
			delete popouts[key];
			this.focusReturnMeta.delete(key);
			delete this.closingPopouts[key];
		}
	}

	private filterClosingPopouts(popouts: Record<string, PopoutDefinition>, excludeKey?: string): Record<string, true> {
		return Object.keys(popouts).reduce<Record<string, true>>((result, popoutKey) => {
			if (popoutKey !== excludeKey && this.closingPopouts[popoutKey]) {
				result[popoutKey] = true;
			}
			return result;
		}, {});
	}

	private scheduleFocus(meta: FocusRestoreMeta | null): void {
		const retries = 5;
		logger.debug(
			`Popout.scheduleFocus target=${meta?.target ? meta.target.tagName : 'null'} keyboardMode=${meta?.keyboardModeEnabled ?? false}`,
		);
		if (!meta?.restoreFocus || !meta.target) return;
		const {target, keyboardModeEnabled} = meta;
		queueMicrotask(() => {
			const hasHiddenAncestor = (element: HTMLElement): boolean =>
				Boolean(element.closest('[aria-hidden="true"], [data-floating-ui-inert]'));
			const attemptFocus = (remainingRetries: number): void => {
				if (!target.isConnected) {
					logger.debug('Popout.scheduleFocus aborted: target disconnected');
					return;
				}
				if (hasHiddenAncestor(target) && remainingRetries > 0) {
					requestAnimationFrame(() => attemptFocus(remainingRetries - 1));
					return;
				}
				try {
					target.focus({preventScroll: true});
					logger.debug('Popout.scheduleFocus applied focus to target');
				} catch (error) {
					logger.error('Popout.scheduleFocus failed to focus target', error as Error);
					return;
				}
				if (keyboardModeEnabled) {
					logger.debug('Popout.scheduleFocus re-entering keyboard mode');
					KeyboardMode.enterKeyboardMode(false);
				}
			};
			attemptFocus(retries);
		});
	}

	private normalizeKey(key: PopoutKey | string): string {
		return typeof key === 'string' ? key : key.toString();
	}
}

export default new Popout();
