// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {currentInstanceTarget} from '@app/features/platform/transport/InstanceHTTP';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {i18n} from '@lingui/core';

const logger = new Logger('ShortLinkAcceptModalOpener');

function openAcceptModal(kind: string, open: () => Promise<void>): void {
	void open().catch((error: unknown) => {
		logger.error(`Failed to open the ${kind} accept modal`, error);
	});
}

export function openInvite(code: string): void {
	openAcceptModal('invite', async () => {
		const commands = await import('@app/features/invite/commands/InviteCommands');
		await commands.openAcceptModal(code, currentInstanceTarget());
	});
}

export function openGift(code: string): void {
	openAcceptModal('gift', async () => {
		const commands = await import('@app/features/gift/commands/GiftCommands');
		await commands.openAcceptModal(code, currentInstanceTarget());
	});
}

export function openTheme(themeId: string): void {
	openAcceptModal('theme', async () => {
		const commands = await import('@app/features/theme/commands/ThemeCommands');
		commands.openAcceptModal(themeId, i18n, RuntimeConfig.getSnapshot());
	});
}
