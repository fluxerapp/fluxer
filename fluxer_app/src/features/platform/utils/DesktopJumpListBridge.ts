// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import Authentication from '@app/features/auth/state/Authentication';
import {CreateDMModal} from '@app/features/channel/components/modals/CreateDMModal';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import {reaction} from 'mobx';
import React from 'react';

export function startDesktopJumpListBridge(): (() => void) | undefined {
	const electronApi = getElectronAPI();
	if (!electronApi || typeof electronApi.onJumpListNewDm !== 'function') return;
	const disposeAvailability = reaction(
		() => !Authentication.isAuthenticated || RuntimeConfig.dmPickerAvailable,
		(available) => electronApi.setDirectMessagesAvailable?.(available),
		{fireImmediately: true, name: 'DesktopJumpListBridge-directMessages'},
	);
	const disposeTask = electronApi.onJumpListNewDm(() => {
		if (!Authentication.isAuthenticated || !RuntimeConfig.dmPickerAvailable) return;
		ModalCommands.push(ModalCommands.modal(() => React.createElement(CreateDMModal)));
	});
	return () => {
		disposeAvailability();
		disposeTask();
	};
}
