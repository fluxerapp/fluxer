// SPDX-License-Identifier: AGPL-3.0-or-later

import {showGenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModalCommands';
import {SOMETHING_WENT_WRONG_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {getElectronAPI, isDesktop} from '@app/features/ui/utils/NativeUtils';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const COPIED_TO_CLIPBOARD_DESCRIPTOR = msg({
	comment: 'Toast shown after copying text to the clipboard succeeds.',
	message: 'Copied to clipboard',
});
const FAILED_TO_COPY_TO_CLIPBOARD_DESCRIPTOR = msg({
	comment: 'Toast shown after copying text to the clipboard fails.',
	message: 'Failed to copy to clipboard',
});
const logger = new Logger('Clipboard');
export async function writeClipboardText(text: string): Promise<void> {
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
}

export async function copy(i18n: I18n, text: string, suppressToast = false): Promise<boolean> {
	try {
		logger.debug('Copying text to clipboard');
		if (!isDesktop()) {
			logger.debug('Desktop runtime not detected; continuing with web clipboard');
		}
		await writeClipboardText(text);
		logger.debug('Text successfully copied to clipboard');
		if (!suppressToast) {
			ToastCommands.createToast({
				type: 'success',
				children: i18n._(COPIED_TO_CLIPBOARD_DESCRIPTOR),
			});
		}
		return true;
	} catch (error) {
		logger.error('Failed to copy text to clipboard:', error);
		if (!suppressToast) {
			showGenericErrorModal({
				title: () => i18n._(SOMETHING_WENT_WRONG_DESCRIPTOR),
				message: () => i18n._(FAILED_TO_COPY_TO_CLIPBOARD_DESCRIPTOR),
				dataFlx: 'ui.text-copy-commands.copy-error-modal',
			});
		}
		return false;
	}
}
