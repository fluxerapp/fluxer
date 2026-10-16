// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import styles from '@app/features/app/components/dialogs/LoadableSettingsModals.module.css';
import type {ChannelSettingsModal as ChannelSettingsModalComponent} from '@app/features/channel/components/modals/ChannelSettingsModal';
import type {GuildSettingsModal as GuildSettingsModalComponent} from '@app/features/guild/components/modals/GuildSettingsModal';
import {CLOSE_DESCRIPTOR, TRY_AGAIN_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {
	createNamedLoadableComponent,
	type LoadableErrorProps,
} from '@app/features/platform/components/loadable/LoadableComponent';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {Spinner, SpinnerSize} from '@app/features/ui/components/Spinner';
import LayerManager from '@app/features/ui/state/LayerManager';
import {ModalStackContext} from '@app/features/ui/utils/ModalStackContext';
import type {UserSettingsModal as UserSettingsModalComponent} from '@app/features/user/components/modals/UserSettingsModal';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useContext, useEffect, useId} from 'react';

const SettingsModalPending = observer(function SettingsModalPending() {
	const {isVisible, needsBackdrop} = useContext(ModalStackContext);
	const layerKey = useId();
	useEffect(() => {
		LayerManager.addLayer('modal', layerKey, ModalCommands.pop);
		return () => {
			LayerManager.removeLayer('modal', layerKey);
		};
	}, [layerKey]);
	if (!isVisible) {
		return null;
	}
	return (
		<div className={styles.overlay} data-flx="app.loadable-settings-modals.settings-modal-pending.overlay">
			{needsBackdrop && (
				<div className={styles.backdrop} data-flx="app.loadable-settings-modals.settings-modal-pending.backdrop" />
			)}
			<Spinner size={SpinnerSize.LARGE} data-flx="app.loadable-settings-modals.settings-modal-pending.spinner" />
		</div>
	);
});

function SettingsModalUnavailable({retry}: LoadableErrorProps): React.JSX.Element {
	const {i18n} = useLingui();
	return (
		<ConfirmModal
			title={<Trans>Couldn't load settings</Trans>}
			description={<Trans>Check your connection and try again.</Trans>}
			primaryText={i18n._(TRY_AGAIN_DESCRIPTOR)}
			onPrimary={retry}
			secondaryText={i18n._(CLOSE_DESCRIPTOR)}
			disableAutoDismiss
			data-flx="app.loadable-settings-modals.settings-modal-unavailable.confirm-modal"
		/>
	);
}

export const UserSettingsModal = createNamedLoadableComponent<React.ComponentProps<typeof UserSettingsModalComponent>>({
	displayName: 'UserSettingsModal',
	load: async () => (await import('@app/features/user/components/modals/UserSettingsModal')).UserSettingsModal,
	LoadingComponent: SettingsModalPending,
	ErrorComponent: SettingsModalUnavailable,
});

export const GuildSettingsModal = createNamedLoadableComponent<
	React.ComponentProps<typeof GuildSettingsModalComponent>
>({
	displayName: 'GuildSettingsModal',
	load: async () => (await import('@app/features/guild/components/modals/GuildSettingsModal')).GuildSettingsModal,
	LoadingComponent: SettingsModalPending,
	ErrorComponent: SettingsModalUnavailable,
});

export const ChannelSettingsModal = createNamedLoadableComponent<
	React.ComponentProps<typeof ChannelSettingsModalComponent>
>({
	displayName: 'ChannelSettingsModal',
	load: async () => (await import('@app/features/channel/components/modals/ChannelSettingsModal')).ChannelSettingsModal,
	LoadingComponent: SettingsModalPending,
	ErrorComponent: SettingsModalUnavailable,
});
