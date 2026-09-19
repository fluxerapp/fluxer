// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {MenuGroupLabel} from '@app/features/ui/action_menu/ContextMenu';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import styles from '@app/features/voice/components/ActiveScreenShareMenu.module.css';
import {
	openScreenShareSourceSwitcherModal,
	type ScreenSharePickerTab,
} from '@app/features/voice/components/modals/ScreenSharePickerModal';
import {StreamSettingsMenuContent} from '@app/features/voice/components/StreamSettingsMenuContent';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import ScreenShareDelivery from '@app/features/voice/state/ScreenShareDelivery';
import type {DisplayShareEnvironment} from '@app/features/voice/utils/ScreenShareEnvironment';
import {isScreenShareRollbackIncompleteError} from '@app/features/voice/utils/ScreenShareRollbackIncompleteError';
import {handleScreenShareError} from '@app/features/voice/utils/ScreenShareUtils';
import type {StreamSettingsShareContext} from '@app/features/voice/utils/StreamSettingsUpdatePolicy';
import {
	formatScreenShareDeliveryStatus,
	SCREEN_SHARE_TRY_FULL_QUALITY_AGAIN_DESCRIPTOR,
} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ArrowCounterClockwiseIcon, MonitorPlayIcon, StopCircleIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const STOP_STREAMING_DESCRIPTOR = msg({
	message: 'Stop streaming',
	comment: 'Danger action that stops the active screen share.',
});
const CHANGE_STREAM_DESCRIPTOR = msg({
	message: 'Change stream',
	comment: 'Action that opens the source picker for the active screen share.',
});
const logger = new Logger('ActiveScreenShareMenu');

export interface ActiveScreenShareMenuProps {
	onClose: () => void;
	displayShareEnvironment: DisplayShareEnvironment;
	shareContext: StreamSettingsShareContext;
	shareContextResolved: boolean;
	iconClassName?: string;
	additionalActions?: React.ReactNode;
	showLiveSettings?: boolean;
	tail?: React.ReactNode;
}

export async function stopActiveScreenShare(): Promise<void> {
	await MediaEngine.setScreenShareEnabled(false);
}

function getSourceSwitcherTab(shareContext: StreamSettingsShareContext): ScreenSharePickerTab {
	if (shareContext === 'device') return 'devices';
	if (shareContext === 'app') return 'apps';
	return 'displays';
}

export async function changeActiveScreenShare(shareContext: StreamSettingsShareContext = 'display'): Promise<void> {
	await openScreenShareSourceSwitcherModal({initialTab: getSourceSwitcherTab(shareContext)});
}

const ActiveScreenShareStatusGroup: React.FC<{
	onClose: () => void;
	iconClassName?: string;
	'data-flx': string;
}> = observer(({onClose, iconClassName, 'data-flx': dataFlx}) => {
	const {i18n} = useLingui();
	if (!ScreenShareDelivery.adaptive) return null;
	const status = formatScreenShareDeliveryStatus(i18n, ScreenShareDelivery.plan, ScreenShareDelivery.notice);
	return (
		<MenuGroup data-flx="voice.active-screen-share-menu.status">
			{status.hasRows && (
				<MenuGroupLabel className={styles.statusLabel} data-flx={dataFlx}>
					{status.sending !== null && (
						<span className={styles.statusRow} data-flx="voice.active-screen-share-menu.status-sending">
							{status.sending}
						</span>
					)}
					{status.askedFor !== null && (
						<span className={styles.statusRow} data-flx="voice.active-screen-share-menu.status-asked-for">
							{status.askedFor}
						</span>
					)}
					{status.notice !== null && (
						<span
							className={clsx(styles.statusRow, styles.statusNotice)}
							data-flx="voice.active-screen-share-menu.status-notice"
						>
							{status.notice}
						</span>
					)}
					{status.sourceRate !== null && (
						<span className={styles.statusRow} data-flx="voice.active-screen-share-menu.status-source-rate">
							{status.sourceRate}
						</span>
					)}
				</MenuGroupLabel>
			)}
			{status.canTryFullQuality && (
				<MenuItem
					icon={
						<ArrowCounterClockwiseIcon
							weight="fill"
							className={iconClassName}
							data-flx="voice.active-screen-share-menu.try-full-quality-icon"
						/>
					}
					onClick={() => {
						onClose();
						void MediaEngine.resetScreenShareDeliveryToFullQuality().catch((error) => {
							logger.error('Failed to restart the screen share at full quality', error);
						});
					}}
					data-flx="voice.active-screen-share-menu.try-full-quality"
				>
					{i18n._(SCREEN_SHARE_TRY_FULL_QUALITY_AGAIN_DESCRIPTOR)}
				</MenuItem>
			)}
		</MenuGroup>
	);
});

export const ActiveScreenShareMenu: React.FC<ActiveScreenShareMenuProps> = ({
	onClose,
	displayShareEnvironment,
	shareContext,
	shareContextResolved,
	iconClassName,
	additionalActions,
	showLiveSettings = true,
	tail,
}) => {
	const {i18n} = useLingui();
	const isWeb = displayShareEnvironment === 'web';
	const openChangeStream = (nextShareContext: StreamSettingsShareContext) => {
		onClose();
		void changeActiveScreenShare(nextShareContext).catch((error) => {
			logger.error('Failed to change active screen share source', error);
		});
	};
	const handleChangeStream = () => openChangeStream(shareContext);
	return (
		<>
			<ActiveScreenShareStatusGroup
				onClose={onClose}
				iconClassName={iconClassName}
				data-flx="voice.active-screen-share-menu.status-label"
			/>
			<MenuGroup data-flx="voice.active-screen-share-menu.actions">
				<MenuItem
					icon={
						<StopCircleIcon
							weight="fill"
							className={iconClassName}
							data-flx="voice.active-screen-share-menu.stop-circle-icon"
						/>
					}
					danger
					onClick={() => {
						onClose();
						void stopActiveScreenShare().catch((error) => {
							if (isScreenShareRollbackIncompleteError(error)) handleScreenShareError(error);
							logger.error('Failed to stop active screen share', error);
						});
					}}
					data-flx="voice.active-screen-share-menu.stop"
				>
					{i18n._(STOP_STREAMING_DESCRIPTOR)}
				</MenuItem>
				<MenuItem
					icon={
						<MonitorPlayIcon
							weight="fill"
							className={iconClassName}
							data-flx="voice.active-screen-share-menu.monitor-play-icon"
						/>
					}
					onClick={handleChangeStream}
					data-flx="voice.active-screen-share-menu.change"
				>
					{i18n._(CHANGE_STREAM_DESCRIPTOR)}
				</MenuItem>
				{additionalActions}
				{showLiveSettings && !isWeb && (
					<StreamSettingsMenuContent
						applyToLiveStream
						variant="compactLive"
						displayShareEnvironment={displayShareEnvironment}
						shareContext={shareContext}
						shareContextResolved={shareContextResolved}
						data-flx="voice.active-screen-share-menu.live-settings"
					/>
				)}
			</MenuGroup>
			{tail}
		</>
	);
};
