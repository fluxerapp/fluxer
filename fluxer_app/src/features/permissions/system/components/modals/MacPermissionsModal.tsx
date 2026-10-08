// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {MacPermissionPrompt} from '@app/features/permissions/system/components/MacPermissionPrompt';
import styles from '@app/features/permissions/system/components/modals/MacPermissionsModal.module.css';
import {macPermissionNameDescriptor} from '@app/features/permissions/system/components/useMacPermissionControl';
import type {MacPermissionKind} from '@app/features/permissions/system/state/MacPermissions';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {useLingui} from '@lingui/react/macro';
import type React from 'react';
import {useCallback, useRef} from 'react';

export const MAC_PERMISSIONS_MODAL_KEY = 'mac-permissions';

interface MacPermissionsModalProps {
	readonly focus: MacPermissionKind;
}

export const MacPermissionsModal: React.FC<MacPermissionsModalProps> = ({focus}) => {
	const {i18n} = useLingui();
	const closedRef = useRef(false);
	const initialFocusRef = useRef<HTMLButtonElement | null>(null);
	const close = useCallback(() => {
		if (closedRef.current) return;
		closedRef.current = true;
		ModalCommands.popWithKey(MAC_PERMISSIONS_MODAL_KEY);
	}, []);
	return (
		<Modal.Root
			size="small"
			centered
			onClose={close}
			initialFocusRef={initialFocusRef}
			data-flx="permissions.mac-permissions-modal.root"
		>
			<Modal.ScreenReaderLabel
				text={i18n._(macPermissionNameDescriptor(focus))}
				data-flx="permissions.mac-permissions-modal.screen-reader-label"
			/>
			<div className={styles.content} data-flx="permissions.mac-permissions-modal.content">
				<MacPermissionPrompt
					kind={focus}
					actionRef={initialFocusRef}
					onAllowed={close}
					onNotNow={close}
					data-flx="permissions.mac-permissions-modal.mac-permission-prompt"
				/>
			</div>
		</Modal.Root>
	);
};
