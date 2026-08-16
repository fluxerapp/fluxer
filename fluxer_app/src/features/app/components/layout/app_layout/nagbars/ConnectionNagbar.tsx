// SPDX-License-Identifier: AGPL-3.0-or-later

import {switchAccountFromStalledConnection} from '@app/features/app/ConnectionRecovery';
import styles from '@app/features/app/components/layout/app_layout/nagbars/ConnectionNagbar.module.css';
import {Nagbar} from '@app/features/app/components/layout/Nagbar';
import {NagbarButton} from '@app/features/app/components/layout/NagbarButton';
import {NagbarContent} from '@app/features/app/components/layout/NagbarContent';
import {
	NAGBAR_TONE_MAINTENANCE,
	NAGBAR_TONE_NEUTRAL,
	type NagbarTone,
} from '@app/features/app/components/layout/NagbarTones';
import {NagbarSkeleton} from '@app/features/app/components/skeleton/NagbarSkeleton';
import type {RememberedSkeletonNagbarRow} from '@app/features/app/components/skeleton/SkeletonLayoutMemory';
import {
	type ConnectionNotice,
	ConnectionNoticeTone,
	useConnectionNotice,
} from '@app/features/app/hooks/useConnectionNotice';
import AccountSwitcherModal from '@app/features/auth/components/accounts/AccountSwitcherModal';
import {Typing} from '@app/features/channel/components/ChannelTyping';
import {openExternalUrlWithWarning} from '@app/features/messaging/utils/ExternalLinkUtils';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback} from 'react';

const CONNECTION_NAGBAR_TONES: Record<ConnectionNoticeTone, NagbarTone> = {
	[ConnectionNoticeTone.NEUTRAL]: NAGBAR_TONE_NEUTRAL,
	[ConnectionNoticeTone.MAINTENANCE]: NAGBAR_TONE_MAINTENANCE,
};
const EMPTY_SKELETON_NAGBAR_ROWS: ReadonlyArray<RememberedSkeletonNagbarRow> = Object.freeze([]);
const CONNECTION_ACCOUNT_SWITCHER_MODAL_KEY = 'connection-account-switcher';
const TYPING_INDICATOR_VIEWBOX_SCALE = 20;
const SWITCH_ACCOUNT_DESCRIPTOR = msg({
	message: 'Switch account',
	comment: 'Button on the connection banner that opens the account switcher.',
});

function resolveConnectionActionUrl(notice: ConnectionNotice | null): string | null {
	if (notice == null || notice.action == null) {
		return null;
	}
	return notice.action.url;
}

interface ConnectionNagbarProps {
	readonly isMobile: boolean;
	readonly rememberedRows?: ReadonlyArray<RememberedSkeletonNagbarRow> | null;
}

export const ConnectionNagbar = observer(({isMobile, rememberedRows}: ConnectionNagbarProps) => {
	const skeletonRows = rememberedRows ?? EMPTY_SKELETON_NAGBAR_ROWS;
	const {i18n} = useLingui();
	const notice = useConnectionNotice();
	const actionUrl = resolveConnectionActionUrl(notice);
	const handleOpenActionUrl = useCallback(() => {
		if (actionUrl == null || actionUrl === '') {
			return;
		}
		openExternalUrlWithWarning(actionUrl);
	}, [actionUrl]);
	const handleSwitchAccount = useCallback(() => {
		ModalCommands.pushWithKey(
			modal(() => (
				<AccountSwitcherModal
					redirectAfterLogin={null}
					redirectAfterSwitch={null}
					switchAccount={switchAccountFromStalledConnection}
					data-flx="app.app-layout.nagbars.connection-nagbar.handle-switch-account.account-switcher-modal"
				/>
			)),
			CONNECTION_ACCOUNT_SWITCHER_MODAL_KEY,
		);
	}, []);
	if (notice == null) {
		return (
			<NagbarSkeleton
				isMobile={isMobile}
				rows={skeletonRows}
				data-flx="app.app-layout.nagbars.connection-nagbar.nagbar-skeleton"
			/>
		);
	}
	const activeNotice = notice;
	const tone = CONNECTION_NAGBAR_TONES[activeNotice.tone];
	function renderNoticeAction() {
		if (activeNotice.action == null) {
			return null;
		}
		return (
			<NagbarButton
				isMobile={isMobile}
				variant="ghost"
				onClick={handleOpenActionUrl}
				disabled={false}
				submitting={false}
				data-flx="app.app-layout.nagbars.connection-nagbar.render-notice-action.nagbar-button.open-action-url"
			>
				{activeNotice.action.label}
			</NagbarButton>
		);
	}
	function renderSwitchAccountAction() {
		if (!activeNotice.showSwitchAccount) {
			return null;
		}
		return (
			<NagbarButton
				isMobile={isMobile}
				variant="ghost"
				onClick={handleSwitchAccount}
				disabled={false}
				submitting={false}
				data-flx="app.app-layout.nagbars.connection-nagbar.render-switch-account-action.nagbar-button.switch-account"
			>
				{i18n._(SWITCH_ACCOUNT_DESCRIPTOR)}
			</NagbarButton>
		);
	}
	function renderActions() {
		if (activeNotice.action == null && !activeNotice.showSwitchAccount) {
			return undefined;
		}
		return (
			<>
				{renderNoticeAction()}
				{renderSwitchAccountAction()}
			</>
		);
	}
	return (
		<>
			<Nagbar
				isMobile={isMobile}
				backgroundColor={tone.backgroundColor}
				textColor={tone.textColor}
				data-flx="app.app-layout.nagbars.connection-nagbar.nagbar"
			>
				<NagbarContent
					isMobile={isMobile}
					message={
						<div className={styles.message} data-flx="app.app-layout.nagbars.connection-nagbar.message">
							<Typing
								className={styles.indicator}
								size={TYPING_INDICATOR_VIEWBOX_SCALE}
								color={tone.textColor}
								data-flx="app.app-layout.nagbars.connection-nagbar.indicator"
							/>
							<span data-flx="app.app-layout.nagbars.connection-nagbar.span">{activeNotice.message}</span>
						</div>
					}
					actions={renderActions()}
					data-flx="app.app-layout.nagbars.connection-nagbar.nagbar-content"
				/>
			</Nagbar>
			<NagbarSkeleton
				isMobile={isMobile}
				rows={skeletonRows.slice(1)}
				data-flx="app.app-layout.nagbars.connection-nagbar.nagbar-skeleton--2"
			/>
		</>
	);
});
