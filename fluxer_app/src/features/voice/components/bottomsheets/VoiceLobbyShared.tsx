// SPDX-License-Identifier: AGPL-3.0-or-later

import {CAMERA_ON_DESCRIPTOR, SETTINGS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {
	CameraOffIcon,
	CameraOnIcon,
	DeafenIcon,
	MicrophoneOffIcon,
	MicrophoneOnIcon,
	SettingsIcon,
	UndeafenIcon,
} from '@app/features/ui/action_menu/ContextMenuIcons';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import styles from '@app/features/voice/components/bottomsheets/VoiceLobbyShared.module.css';
import {
	formatMilliseconds,
	formatPacketLossPercent,
} from '@app/features/voice/components/voice_connection_status/shared';
import {getOpenVoiceVideoSettingsLabel} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import type React from 'react';

interface VoiceLobbyControlsProps {
	'data-flx': string;
	muteButtonDataFlx: string;
	isConnected: boolean;
	isMuted: boolean;
	isDeafened: boolean;
	isCameraOn: boolean;
	micLocked?: boolean;
	deafenLocked?: boolean;
	muteLabel: string;
	muteAriaLabel?: string;
	deafenLabel: string;
	deafenAriaLabel?: string;
	cameraCapBlocked: boolean;
	cameraCapBlockedLabel: string;
	cameraOffLabel: string;
	onToggleMute: () => void;
	onToggleDeafen: () => void;
	onToggleCamera: () => void;
	onOpenVoiceSettings: () => void;
}

export function VoiceLobbyControls({
	'data-flx': dataFlx,
	muteButtonDataFlx,
	isConnected,
	isMuted,
	isDeafened,
	isCameraOn,
	micLocked,
	deafenLocked,
	muteLabel,
	muteAriaLabel,
	deafenLabel,
	deafenAriaLabel,
	cameraCapBlocked,
	cameraCapBlockedLabel,
	cameraOffLabel,
	onToggleMute,
	onToggleDeafen,
	onToggleCamera,
	onOpenVoiceSettings,
}: VoiceLobbyControlsProps) {
	const {i18n} = useLingui();
	return (
		<div className={styles.actionButtons} data-flx={`${dataFlx}.action-buttons`}>
			<button
				type="button"
				className={styles.actionButton}
				onClick={micLocked ? undefined : onToggleMute}
				disabled={micLocked}
				aria-label={muteAriaLabel}
				aria-pressed={isMuted}
				data-flx={muteButtonDataFlx}
			>
				<div
					className={clsx(styles.iconContainer, isMuted ? styles.iconContainerDanger : styles.iconContainerBrand)}
					data-flx={`${dataFlx}.icon-container`}
				>
					{isMuted ? (
						<MicrophoneOffIcon className={styles.actionIcon} size={24} data-flx={`${dataFlx}.action-icon`} />
					) : (
						<MicrophoneOnIcon className={styles.actionIcon} size={24} data-flx={`${dataFlx}.action-icon--2`} />
					)}
				</div>
				<span className={styles.actionText} data-flx={`${dataFlx}.action-text`}>
					{muteLabel}
				</span>
			</button>
			<button
				type="button"
				className={styles.actionButton}
				onClick={deafenLocked ? undefined : onToggleDeafen}
				disabled={deafenLocked}
				aria-label={deafenAriaLabel}
				aria-pressed={isDeafened}
				data-flx={`${dataFlx}.action-button.toggle-deafen`}
			>
				<div
					className={clsx(styles.iconContainer, isDeafened ? styles.iconContainerDanger : styles.iconContainerTertiary)}
					data-flx={`${dataFlx}.icon-container--2`}
				>
					{isDeafened ? (
						<DeafenIcon
							className={styles.actionIconSecondary}
							size={24}
							data-flx={`${dataFlx}.action-icon-secondary`}
						/>
					) : (
						<UndeafenIcon
							className={styles.actionIconSecondary}
							size={24}
							data-flx={`${dataFlx}.action-icon-secondary--2`}
						/>
					)}
				</div>
				<span className={styles.actionText} data-flx={`${dataFlx}.action-text--2`}>
					{deafenLabel}
				</span>
			</button>
			{isConnected &&
				(() => {
					const cameraToggleButton = (
						<button
							type="button"
							className={styles.actionButton}
							onClick={cameraCapBlocked ? undefined : onToggleCamera}
							disabled={cameraCapBlocked}
							aria-label={cameraCapBlocked ? cameraCapBlockedLabel : undefined}
							aria-pressed={isCameraOn}
							data-flx={`${dataFlx}.action-button.toggle-camera`}
						>
							<div
								className={clsx(
									styles.iconContainer,
									isCameraOn ? styles.iconContainerSuccess : styles.iconContainerTertiary,
								)}
								data-flx={`${dataFlx}.icon-container--3`}
							>
								{isCameraOn ? (
									<CameraOnIcon className={styles.actionIcon} size={24} data-flx={`${dataFlx}.action-icon--3`} />
								) : (
									<CameraOffIcon
										className={styles.actionIconSecondary}
										size={24}
										data-flx={`${dataFlx}.action-icon-secondary--3`}
									/>
								)}
							</div>
							<span className={styles.actionText} data-flx={`${dataFlx}.action-text--3`}>
								{isCameraOn ? i18n._(CAMERA_ON_DESCRIPTOR) : cameraOffLabel}
							</span>
						</button>
					);
					if (!cameraCapBlocked) return cameraToggleButton;
					return (
						<Tooltip text={cameraCapBlockedLabel} data-flx={`${dataFlx}.tooltip.camera-cap`}>
							{cameraToggleButton}
						</Tooltip>
					);
				})()}
			<button
				type="button"
				className={styles.actionButton}
				onClick={onOpenVoiceSettings}
				aria-label={getOpenVoiceVideoSettingsLabel(i18n)}
				data-flx={`${dataFlx}.action-button.open-voice-settings`}
			>
				<div
					className={clsx(styles.iconContainer, styles.iconContainerTertiary)}
					data-flx={`${dataFlx}.icon-container--4`}
				>
					<SettingsIcon
						className={styles.actionIconSecondary}
						size={24}
						data-flx={`${dataFlx}.action-icon-secondary--4`}
					/>
				</div>
				<span className={styles.actionText} data-flx={`${dataFlx}.action-text--4`}>
					{i18n._(SETTINGS_DESCRIPTOR)}
				</span>
			</button>
		</div>
	);
}

function Row({
	label,
	value,
	valueClassName,
	dataFlx,
}: {
	label: string;
	value: React.ReactNode;
	valueClassName?: string;
	dataFlx: string;
}) {
	return (
		<div className={styles.statRow} data-flx={`${dataFlx}.row.stat-row`}>
			<span className={styles.statLabel} data-flx={`${dataFlx}.row.stat-label`}>
				{label}
			</span>
			<div className={clsx(styles.statValue, valueClassName)} data-flx={`${dataFlx}.row.stat-value`}>
				{value}
			</div>
		</div>
	);
}

interface VoiceLobbyConnectionStatsProps {
	'data-flx': string;
	title: string;
	subtitle: string;
	labels: {ping: string; endpoint: string; connectionId: string; packetLoss: string; jitter: string};
	currentLatency: number | null;
	voiceServerEndpoint: string | null;
	connectionId: string | null;
	audioPacketLoss: number | undefined;
	jitter: number | undefined;
}

export function VoiceLobbyConnectionStats({
	'data-flx': dataFlx,
	title,
	subtitle,
	labels,
	currentLatency,
	voiceServerEndpoint,
	connectionId,
	audioPacketLoss,
	jitter,
}: VoiceLobbyConnectionStatsProps) {
	const {i18n} = useLingui();
	const prettyEndpoint = (() => {
		if (!voiceServerEndpoint) return null;
		try {
			const url = new URL(voiceServerEndpoint);
			return url.port ? `${url.hostname}:${url.port}` : url.hostname;
		} catch {
			return voiceServerEndpoint;
		}
	})();
	return (
		<div className={styles.connectionInfo} data-flx={`${dataFlx}.connection-info`}>
			<div className={styles.connectionHeader} data-flx={`${dataFlx}.connection-header`}>
				<div className={styles.connectionStatusInfo} data-flx={`${dataFlx}.connection-status-info`}>
					<div className={styles.connectionTitle} data-flx={`${dataFlx}.connection-title`}>
						{title}
					</div>
					<div className={styles.connectionSubtitle} data-flx={`${dataFlx}.connection-subtitle`}>
						{subtitle}
					</div>
				</div>
				<div className={styles.connectionStatusDot} aria-hidden="true" data-flx={`${dataFlx}.connection-status-dot`} />
			</div>
			<div className={styles.statsGrid} data-flx={`${dataFlx}.stats-grid`}>
				{currentLatency !== null && (
					<Row
						dataFlx={dataFlx}
						label={labels.ping}
						value={
							<span className={styles.statValuePrimary} data-flx={`${dataFlx}.stat-value-primary`}>
								{formatMilliseconds(currentLatency, i18n.locale)}
							</span>
						}
						data-flx={`${dataFlx}.row`}
					/>
				)}
				{prettyEndpoint && (
					<Row
						dataFlx={dataFlx}
						label={labels.endpoint}
						value={
							<Tooltip text={prettyEndpoint} data-flx={`${dataFlx}.tooltip`}>
								<span className={styles.endpointValue} data-flx={`${dataFlx}.endpoint-value`}>
									{prettyEndpoint}
								</span>
							</Tooltip>
						}
						valueClassName={styles.maxWidth}
						data-flx={`${dataFlx}.row--2`}
					/>
				)}
				{connectionId && (
					<Row
						dataFlx={dataFlx}
						label={labels.connectionId}
						value={
							<Tooltip text={connectionId} data-flx={`${dataFlx}.tooltip--2`}>
								<span className={styles.connectionIdValue} data-flx={`${dataFlx}.connection-id-value`}>
									{connectionId}
								</span>
							</Tooltip>
						}
						valueClassName={styles.maxWidth}
						data-flx={`${dataFlx}.row--3`}
					/>
				)}
				{typeof audioPacketLoss === 'number' && audioPacketLoss > 0 && (
					<Row
						dataFlx={dataFlx}
						label={labels.packetLoss}
						value={
							<span className={styles.statValuePrimary} data-flx={`${dataFlx}.stat-value-primary--2`}>
								{formatPacketLossPercent(audioPacketLoss, i18n.locale)}
							</span>
						}
						data-flx={`${dataFlx}.row--4`}
					/>
				)}
				{typeof jitter === 'number' && jitter > 0 && (
					<Row
						dataFlx={dataFlx}
						label={labels.jitter}
						value={
							<span className={styles.statValuePrimary} data-flx={`${dataFlx}.stat-value-primary--3`}>
								{formatMilliseconds(jitter, i18n.locale)}
							</span>
						}
						data-flx={`${dataFlx}.row--5`}
					/>
				)}
			</div>
		</div>
	);
}
