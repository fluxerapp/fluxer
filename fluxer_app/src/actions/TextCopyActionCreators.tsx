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

import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {Logger} from '~/lib/Logger';
import {getElectronAPI, isDesktop} from '~/utils/NativeUtils';

const logger = new Logger('Clipboard');

const writeWithFallback = async (text: string): Promise<void> => {
	const electronApi = getElectronAPI();
	if (electronApi?.clipboardWriteText) {
		logger.debug('Using Electron clipboard');
		await electronApi.clipboardWriteText(text);
		return;
	}

	if (navigator.clipboard?.writeText) {
		logger.debug('Using navigator.clipboard');
		await navigator.clipboard.writeText(text);
		return;
	}

	logger.debug('Falling back to temporary textarea copy');
	const textarea = document.createElement('textarea');
	textarea.value = text;
	textarea.style.position = 'fixed';
	textarea.style.opacity = '0';
	document.body.appendChild(textarea);
	textarea.focus();
	textarea.select();
	const success = document.execCommand('copy');
	document.body.removeChild(textarea);
	if (success) return;

	throw new Error('No clipboard API available');
};

export const copy = async (i18n: I18n, text: string, suppressToast = false): Promise<boolean> => {
	try {
		logger.debug('Copying text to clipboard');
		if (!isDesktop()) {
			logger.debug('Desktop runtime not detected; continuing with web clipboard');
		}
		await writeWithFallback(text);
		logger.debug('Text successfully copied to clipboard');
		if (!suppressToast) {
			ToastActionCreators.createToast({type: 'success', children: i18n._(msg`Copied to clipboard`)});
		}
		return true;
	} catch (error) {
		logger.error('Failed to copy text to clipboard:', error);
		if (!suppressToast) {
			ToastActionCreators.createToast({type: 'error', children: i18n._(msg`Failed to copy to clipboard`)});
		}
		return false;
	}
};
