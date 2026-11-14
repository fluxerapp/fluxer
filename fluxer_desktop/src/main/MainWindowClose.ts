// SPDX-License-Identifier: AGPL-3.0-or-later

export interface MainWindowCloseContext {
	platform: NodeJS.Platform;
	isQuitting: boolean;
	closingForUpdate: boolean;
	keepInTray: boolean;
}

export type MainWindowCloseAction = 'hide' | 'close' | 'quit';

export function getMainWindowCloseAction(context: MainWindowCloseContext): MainWindowCloseAction {
	if (context.isQuitting || context.closingForUpdate) {
		return 'close';
	}
	if (context.platform === 'darwin' || context.keepInTray) {
		return 'hide';
	}
	return 'quit';
}
