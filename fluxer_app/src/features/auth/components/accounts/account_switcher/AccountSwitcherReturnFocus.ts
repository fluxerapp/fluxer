// SPDX-License-Identifier: AGPL-3.0-or-later

import Accounts from '@app/features/auth/state/Accounts';
import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import {when} from 'mobx';

const MAIN_CONTENT_ID = 'main-content';
const NEW_ACCOUNT_VIEW_TIMEOUT_MS = 5000;
const COMPOSER_RETRY_DELAY_MS = 300;

interface SwitcherReturnFocusRequest {
	readonly overlay: HTMLElement | null;
	readonly returnTarget: HTMLElement | null;
	readonly switchedAccount: boolean;
}

function asHTMLElement(node: Element | null, ownerDocument: Document): HTMLElement | null {
	const view = ownerDocument.defaultView;
	if (node == null || view == null || !(node instanceof view.HTMLElement)) return null;
	return node;
}

export function captureSwitcherReturnTarget(ownerDocument: Document): HTMLElement | null {
	const active = asHTMLElement(ownerDocument.activeElement, ownerDocument);
	return active === ownerDocument.body ? null : active;
}

function focusIfPossible(element: HTMLElement | null): boolean {
	if (element == null || !element.isConnected || element.closest('[inert]') != null) return false;
	element.focus({preventScroll: true});
	return element.ownerDocument.activeElement === element;
}

function focusNewAccountView(ownerDocument: Document): void {
	const mainContent = asHTMLElement(ownerDocument.getElementById(MAIN_CONTENT_ID), ownerDocument);
	const active = ownerDocument.activeElement;
	if (active != null && active !== ownerDocument.body && active !== mainContent && active.isConnected) return;
	focusIfPossible(mainContent);
	ComponentBus.dispatch('FOCUS_TEXTAREA');
}

function focusNewAccountViewOnceLive(ownerDocument: Document): void {
	const view = ownerDocument.defaultView;
	if (view == null) return;
	void when(() => Accounts.isViewLive, {timeout: NEW_ACCOUNT_VIEW_TIMEOUT_MS})
		.catch(() => {})
		.then(() => {
			view.requestAnimationFrame(() => focusNewAccountView(ownerDocument));
			view.setTimeout(() => focusNewAccountView(ownerDocument), COMPOSER_RETRY_DELAY_MS);
		});
}

export function focusNewAccountViewAfterSwitch(ownerDocument: Document): void {
	focusIfPossible(asHTMLElement(ownerDocument.getElementById(MAIN_CONTENT_ID), ownerDocument));
	focusNewAccountViewOnceLive(ownerDocument);
}

export function restoreFocusAfterSwitcher(
	ownerDocument: Document,
	{overlay, returnTarget, switchedAccount}: SwitcherReturnFocusRequest,
): void {
	const active = asHTMLElement(ownerDocument.activeElement, ownerDocument);
	const focusIsOutside =
		active != null && active !== ownerDocument.body && active.isConnected && overlay?.contains(active) !== true;
	if (focusIsOutside && active.closest('[aria-modal="true"]') != null) return;
	if (switchedAccount) {
		focusNewAccountViewAfterSwitch(ownerDocument);
		return;
	}
	if (focusIsOutside || focusIfPossible(returnTarget)) return;
	focusIfPossible(asHTMLElement(ownerDocument.getElementById(MAIN_CONTENT_ID), ownerDocument));
}
