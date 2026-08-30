// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import {
	openScreenShareSourceSwitcherModal,
	type ScreenSharePickerTab,
} from '@app/features/voice/components/modals/ScreenSharePickerModal';
import {StreamSettingsMenuContent} from '@app/features/voice/components/StreamSettingsMenuContent';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import type {DisplayShareEnvironment} from '@app/features/voice/utils/ScreenShareEnvironment';
import type {StreamSettingsShareContext} from '@app/features/voice/utils/StreamSettingsUpdatePolicy';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {MonitorPlayIcon, StopCircleIcon} from '@phosphor-icons/react';
import type React from 'react';

const STOP_STREAMING_DESCRIPTOR = msg({
	message: 'Stop Streaming',
	comment: 'Danger action that stops the active screen share.',
});
const CHANGE_STREAM_DESCRIPTOR = msg({
	message: 'Change Stream',
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
