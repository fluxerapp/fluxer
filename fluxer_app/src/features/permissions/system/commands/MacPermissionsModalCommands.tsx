// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type MediaPermissionBlockedKind,
	MediaPermissionBlockedModal,
} from '@app/features/permissions/system/components/MediaPermissionBlockedModal';
import type * as MacPermissionsModalModule from '@app/features/permissions/system/components/modals/MacPermissionsModal';
import type {MacPermissionKind} from '@app/features/permissions/system/state/MacPermissions';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {modal, push, pushWithKey} from '@app/features/ui/commands/ModalCommands';
import {getNativePlatformSync, isDesktop} from '@app/features/ui/utils/NativeUtils';

const logger = new Logger('MacPermissionsModalCommands');

function loadMacPermissionsModal(): Promise<typeof MacPermissionsModalModule> {
	return import('@app/features/permissions/system/components/modals/MacPermissionsModal');
}

export function preloadMacPermissionsModal(): void {
	if (!isDesktop() || getNativePlatformSync() !== 'macos') return;
	void loadMacPermissionsModal().catch((error) => {
		logger.debug('Failed to preload the macOS permissions modal', error);
	});
}

export function openMacPermissionsModal(focus: MacPermissionKind): void {
	void loadMacPermissionsModal().then(
		({MAC_PERMISSIONS_MODAL_KEY, MacPermissionsModal}) => {
			pushWithKey(
				modal(() => (
					<MacPermissionsModal
						focus={focus}
						data-flx="permissions.system.mac-permissions-modal-commands.open-mac-permissions-modal.mac-permissions-modal"
					/>
				)),
				MAC_PERMISSIONS_MODAL_KEY,
			);
		},
		(error) => {
			logger.warn('Failed to load the macOS permissions modal', error);
		},
	);
}

export function handleMediaPermissionBlocked(kind: MediaPermissionBlockedKind): void {
	if (isDesktop() && getNativePlatformSync() === 'macos') {
		openMacPermissionsModal(kind);
		return;
	}
	push(
		modal(() => (
			<MediaPermissionBlockedModal
				kind={kind}
				data-flx="permissions.system.mac-permissions-modal-commands.handle-media-permission-blocked.media-permission-blocked-modal"
			/>
		)),
	);
}
