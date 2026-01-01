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

import {app, ipcMain, type Session, shell, type WebContents} from 'electron';
import log from 'electron-log';

type LanguageCode = string;

interface SpellcheckState {
	enabled: boolean;
	languages: Array<LanguageCode>;
}

interface RendererSpellcheckState {
	enabled?: boolean;
	languages?: Array<LanguageCode>;
}

const defaultState: SpellcheckState = {
	enabled: true,
	languages: [],
};

const isMac = process.platform === 'darwin';
const isWindows = process.platform === 'win32';
const showSpellMenu = isMac || isWindows;

const normalizeLanguage = (code: string): string => code.toLowerCase();

const contextSourceByWebContents = new WeakMap<WebContents, {isTextarea: boolean; ts: number}>();
let contextIpcRegistered = false;

const ensureContextIpc = () => {
	if (contextIpcRegistered) return;
	contextIpcRegistered = true;

	ipcMain.on('spellcheck-context-target', (event, payload: {isTextarea?: boolean}) => {
		contextSourceByWebContents.set(event.sender, {
			isTextarea: Boolean(payload?.isTextarea),
			ts: Date.now(),
		});
	});
};

const pickSystemLanguages = (session: Session): Array<LanguageCode> => {
	const available = session.availableSpellCheckerLanguages ?? [];
	const availableMap = new Map(available.map((code) => [normalizeLanguage(code), code]));

	const preferred = (typeof app.getPreferredSystemLanguages === 'function' && app.getPreferredSystemLanguages()) || [
		app.getLocale(),
	];

	const selected: Array<LanguageCode> = [];
	for (const lang of preferred) {
		const normalized = normalizeLanguage(lang);
		const exact = availableMap.get(normalized);
		if (exact) {
			selected.push(exact);
		}
	}

	if (selected.length > 0) return selected;
	if (available.length > 0) return [available[0]];
	return [];
};

const applyStateToSession = (session: Session, state: SpellcheckState): void => {
	session.setSpellCheckerEnabled(state.enabled);

	if (!isMac) {
		const languages =
			state.languages.length > 0
				? state.languages
				: pickSystemLanguages(session) || session.availableSpellCheckerLanguages;
		if (languages && languages.length > 0) {
			session.setSpellCheckerLanguages(languages);
		}
	}
};

const shouldHandleContextMenu = (webContents: WebContents, params: Electron.ContextMenuParams): boolean => {
	if (!params.isEditable) return false;

	const inputFieldType = (params as {inputFieldType?: string}).inputFieldType;
	const isPassword =
		(params as {isPassword?: boolean}).isPassword === true ||
		inputFieldType === 'password' ||
		(params as {formControlType?: string}).formControlType === 'password';
	if (isPassword) return false;

	const target = contextSourceByWebContents.get(webContents);
	const targetRecent = target && Date.now() - target.ts < 5000;
	const isTextLike = inputFieldType === 'plainText' || inputFieldType === 'textarea' || inputFieldType === undefined;

	return Boolean((targetRecent && target.isTextarea) || isTextLike);
};

export const registerSpellcheck = (webContents: WebContents): void => {
	ensureContextIpc();

	const session = webContents.session;
	let state: SpellcheckState = {...defaultState};

	const pickLanguages = (langs: Array<string>, electronSession: Electron.Session): Array<string> => {
		if (langs.length > 0) {
			return langs;
		}

		if (!isMac) {
			return pickSystemLanguages(electronSession);
		}

		return [];
	};

	const normalizeState = (incoming: RendererSpellcheckState | SpellcheckState): SpellcheckState => {
		const available = session.availableSpellCheckerLanguages ?? [];
		const availableSet = new Set(available.map(normalizeLanguage));
		const langs = (incoming.languages ?? state.languages ?? []).filter((lang) =>
			availableSet.has(normalizeLanguage(lang)),
		);
		const pickedLanguages = pickLanguages(langs, session);

		return {
			enabled: incoming.enabled ?? state.enabled ?? defaultState.enabled,
			languages: pickedLanguages,
		};
	};

	const broadcastState = () => {
		webContents.send('spellcheck-state-changed', state);
	};

	const setState = (next: RendererSpellcheckState | SpellcheckState, opts?: {broadcast?: boolean}) => {
		state = normalizeState(next);
		applyStateToSession(session, state);
		if (opts?.broadcast !== false) {
			broadcastState();
		}
	};

	setState(state, {broadcast: false});

	ipcMain.handle('spellcheck-get-state', () => state);
	ipcMain.handle('spellcheck-set-state', (_event, next: RendererSpellcheckState) => {
		setState(next);
		return state;
	});
	const openLanguageSettings = async () => {
		if (!showSpellMenu) return false;
		try {
			if (isMac) {
				await shell.openExternal('x-apple.systempreferences:com.apple.preference.keyboard');
				return true;
			}
			if (isWindows) {
				await shell.openExternal('ms-settings:regionlanguage');
				return true;
			}
			return false;
		} catch (error) {
			log.warn('[Spellcheck] Failed to open language settings', error);
			return false;
		}
	};
	ipcMain.handle('spellcheck-get-available-languages', () => session.availableSpellCheckerLanguages ?? []);
	ipcMain.handle('spellcheck-open-language-settings', () => openLanguageSettings());
	ipcMain.handle('spellcheck-replace-misspelling', (_event, replacement: string) => {
		webContents.replaceMisspelling(replacement);
	});
	ipcMain.handle('spellcheck-add-word-to-dictionary', (_event, word: string) => {
		session.addWordToSpellCheckerDictionary(word);
	});

	webContents.on('context-menu', (event, params) => {
		if (!shouldHandleContextMenu(webContents, params)) {
			return;
		}

		event.preventDefault();

		const spellcheckEnabled = session.isSpellCheckerEnabled();
		const misspelledWord = params.misspelledWord;
		const suggestions = params.dictionarySuggestions || [];

		webContents.send('textarea-context-menu', {
			misspelledWord: spellcheckEnabled ? misspelledWord : undefined,
			suggestions: spellcheckEnabled && misspelledWord ? suggestions : [],
			editFlags: {
				canUndo: params.editFlags.canUndo,
				canRedo: params.editFlags.canRedo,
				canCut: params.editFlags.canCut,
				canCopy: params.editFlags.canCopy,
				canPaste: params.editFlags.canPaste,
				canSelectAll: params.editFlags.canSelectAll,
			},
			x: params.x,
			y: params.y,
		});
	});
};
