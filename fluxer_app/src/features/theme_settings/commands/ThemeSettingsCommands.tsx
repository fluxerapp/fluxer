// SPDX-License-Identifier: AGPL-3.0-or-later

import {UserSettingsModal} from '@app/features/app/components/dialogs/LoadableSettingsModals';
import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import * as ThemeProfileCommands from '@app/features/theme/commands/ThemeProfileCommands';
import ThemeEditTarget from '@app/features/theme/state/ThemeEditTarget';
import {flushEditorHistory, forgetOpenedCss} from '@app/features/theme_settings/editor/EditorStateCache';
import ThemeSettingsState, {
	QUICK_CSS_EDITOR_FILE,
	type QuickCssEditorPlacement,
	type QuickCssEditorTab,
} from '@app/features/theme_settings/state/ThemeSettingsState';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import ModalState from '@app/features/ui/state/Modal';
import {getElectronAPI, isDesktop} from '@app/features/ui/utils/NativeUtils';
import type {EditorView} from '@codemirror/view';

export const QUICK_CSS_EDITOR_WINDOW_KEY = 'fluxer_quick_css_editor';

interface QuickCssEditorFocusHandle {
	focus: () => void;
	hasFocus: () => boolean;
}

let editorFocusHandle: QuickCssEditorFocusHandle | null = null;
let activeEditorView: EditorView | null = null;
let focusBeforeOpen: HTMLElement | null = null;

export function setQuickCssEditorFocusHandle(handle: QuickCssEditorFocusHandle | null): void {
	editorFocusHandle = handle;
}

export function registerQuickCssEditorView(view: EditorView): () => void {
	activeEditorView = view;
	return () => {
		if (activeEditorView === view) activeEditorView = null;
	};
}

export function insertIntoQuickCssEditor(text: string): void {
	const view = activeEditorView;
	ThemeSettingsState.setEditorTab('css');
	ThemeSettingsState.requestFocus('editor');
	if (!view || view.state.readOnly) return;
	view.dispatch(view.state.replaceSelection(text), {scrollIntoView: true, userEvent: 'input'});
}

export function revertQuickCssEditor(css: string): void {
	const view = activeEditorView;
	if (!view || view.state.doc.toString() === css) return;
	view.dispatch({changes: {from: 0, to: view.state.doc.length, insert: css}, userEvent: 'input.discard'});
}

export function setEditedCustomThemeCss(css: string | null): void {
	ThemeProfileCommands.setCustomThemeCss(css, ThemeEditTarget.target);
}

export function canOpenQuickCssEditor(): boolean {
	return !MobileLayout.enabled;
}

export function canPopOutQuickCssEditor(): boolean {
	return !isDesktop() || getElectronAPI()?.cssEditorWindow === true;
}

export function flushQuickCssWrites(): void {
	ThemeSettingsState.quickCssWriteFlusher?.();
	flushEditorHistory();
}

export function getAppFocusBeforeEditor(): HTMLElement | null {
	return focusBeforeOpen?.isConnected ? focusBeforeOpen : null;
}

function rememberFocus(): void {
	const active = document.activeElement;
	focusBeforeOpen = active instanceof HTMLElement && active !== document.body ? active : null;
}

export function focusQuickCssEditor(): void {
	if (ThemeSettingsState.editorPlacement === 'window') {
		ThemeSettingsState.editorWindow?.focus();
		void getElectronAPI()?.popoutFocus?.(QUICK_CSS_EDITOR_WINDOW_KEY);
	}
	editorFocusHandle?.focus();
}

function returnFocusToApp(): void {
	window.focus();
	const target = focusBeforeOpen;
	if (target?.isConnected) {
		target.focus();
		return;
	}
	const active = document.activeElement;
	if (active instanceof HTMLElement && active.tagName === 'IFRAME') active.blur();
}

function closeSettingsModal(): void {
	if (ModalState.hasModalOfModalType('user-settings', document)) {
		ModalState.popByModalType('user-settings', document);
	}
}

function closeEditorWithClosedWindow(): void {
	if (ThemeSettingsState.editorPlacement === 'window' && ThemeSettingsState.editorWindow?.closed) {
		closeQuickCssEditor();
	}
}

export function openQuickCssEditor(file: string = QUICK_CSS_EDITOR_FILE): void {
	if (!canOpenQuickCssEditor()) return;
	closeSettingsModal();
	closeEditorWithClosedWindow();
	if (ThemeSettingsState.editorOpen) {
		ThemeSettingsState.requestEditorFile(file);
		ThemeSettingsState.setEditorTab('css');
		ThemeSettingsState.requestFocus('editor');
		focusQuickCssEditor();
		return;
	}
	rememberFocus();
	ThemeSettingsState.openEditor(file, canPopOutQuickCssEditor());
}

export function openQuickCssEditorTab(tab: QuickCssEditorTab): void {
	if (!canOpenQuickCssEditor()) return;
	openQuickCssEditor();
	ThemeSettingsState.setEditorTab(tab);
	ThemeSettingsState.requestFocus('tab');
}

export function toggleQuickCssEditorFocus(): void {
	if (!canOpenQuickCssEditor()) return;
	closeEditorWithClosedWindow();
	if (!ThemeSettingsState.editorOpen) {
		rememberFocus();
		ThemeSettingsState.openEditor(QUICK_CSS_EDITOR_FILE, canPopOutQuickCssEditor());
		return;
	}
	if (editorFocusHandle?.hasFocus()) {
		returnFocusToApp();
		return;
	}
	rememberFocus();
	focusQuickCssEditor();
}

export function toggleCustomCss(): void {
	ThemeSettingsState.setCustomCssOff(!ThemeSettingsState.customCssOff);
}

export function moveQuickCssEditor(placement: QuickCssEditorPlacement): void {
	if (placement === 'window' && !canPopOutQuickCssEditor()) return;
	flushQuickCssWrites();
	ThemeSettingsState.moveEditor(placement, ThemeSettingsState.editorGeneration);
}

export function closeQuickCssEditor(generation: number = ThemeSettingsState.editorGeneration): void {
	if (generation !== ThemeSettingsState.editorGeneration || !ThemeSettingsState.editorOpen) return;
	const hadFocus = editorFocusHandle?.hasFocus() ?? false;
	flushQuickCssWrites();
	ThemeSettingsState.closeEditor(generation);
	forgetOpenedCss();
	if (hadFocus) returnFocusToApp();
	focusBeforeOpen = null;
}

export function openThemesSettings(): void {
	if (ModalState.hasModalOfModalType('user-settings')) {
		ComponentBus.dispatch('USER_SETTINGS_TAB_SELECT', {tab: 'themes'});
		return;
	}
	ModalCommands.push(
		modal(
			() => (
				<UserSettingsModal
					initialTab="themes"
					data-flx="theme-settings.theme-settings-commands.open-themes-settings.user-settings-modal"
				/>
			),
			'user-settings',
		),
	);
}
