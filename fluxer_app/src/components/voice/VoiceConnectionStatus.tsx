/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {t} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {
	CameraIcon,
	CameraSlashIcon,
	DesktopIcon,
	DeviceMobileIcon,
	LockSimpleIcon,
	MonitorIcon,
	PhoneXIcon,
	WaveformIcon,
	XIcon,
} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {useCallback} from 'react';
import * as DeveloperOptionsActionCreators from '~/actions/DeveloperOptionsActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as PopoutActionCreators from '~/actions/PopoutActionCreators';
import * as TextCopyActionCreators from '~/actions/TextCopyActionCreators';
import * as VoiceSettingsActionCreators from '~/actions/VoiceSettingsActionCreators';
import {CameraPreviewModalInRoom} from '~/components/modals/CameraPreviewModal';
import {ScreenShareSettingsModal} from '~/components/modals/ScreenShareSettingsModal';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {FocusRingWrapper} from '~/components/uikit/FocusRingWrapper';
import {Popout} from '~/components/uikit/Popout/Popout';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import {usePopout} from '~/hooks/usePopout';
import {Link} from '~/lib/router';
import {Routes} from '~/Routes';
import ChannelStore from '~/stores/ChannelStore';
import DeveloperOptionsStore from '~/stores/DeveloperOptionsStore';
import GuildStore from '~/stores/GuildStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import VoiceSettingsStore from '~/stores/VoiceSettingsStore';
import MediaEngineStore from '~/stores/voice/MediaEngineFacade';
import type {LatencyDataPoint} from '~/stores/voice/VoiceStatsManager';
import * as ChannelUtils from '~/utils/ChannelUtils';
import {executeScreenShareOperation} from '~/utils/ScreenShareUtils';
import {SignalStrengthIcon} from './SignalStrengthIcon';
import styles from './VoiceConnectionStatus.module.css';

const VoiceDetailsPopout = observer(() => {
	const {i18n} = useLingui();
	const latency = MediaEngineStore.currentLatency;
	const averageLatency = MediaEngineStore.averageLatency;
	const latencyHistory = MediaEngineStore.latencyHistory;
	const voiceServerEndpoint = MediaEngineStore.voiceServerEndpoint;
	const connectionId = MediaEngineStore.connectionId;
	const voiceState = MediaEngineStore.getCurrentUserVoiceState();
	const isMobile = voiceState?.is_mobile ?? false;

	const strippedEndpoint = voiceServerEndpoint
		? (() => {
				try {
					const url = new URL(voiceServerEndpoint);
					return url.port ? `${url.hostname}:${url.port}` : url.hostname;
				} catch {
					return voiceServerEndpoint;
				}
			})()
		: null;

	const chartData = latencyHistory.slice(-30);
	const maxLatency = Math.max(...chartData.map((d: LatencyDataPoint) => d.latency), 0) + 10;
	const chartWidth = 300;
	const chartHeight = 120;
	const padding = {top: 10, right: 10, bottom: 20, left: 40};
	const graphWidth = chartWidth - padding.left - padding.right;
	const graphHeight = chartHeight - padding.top - padding.bottom;

	const createLinePath = () => {
		if (chartData.length === 0) return '';

		const points = chartData.map((point: LatencyDataPoint, index: number) => {
			const x = padding.left + (index / Math.max(chartData.length - 1, 1)) * graphWidth;
			const y = padding.top + graphHeight - (point.latency / maxLatency) * graphHeight;
			return `${x},${y}`;
		});

		return `M ${points.join(' L ')}`;
	};

	return (
		<div className={styles.popoutContainer}>
			<div className={styles.popoutHeader}>
				<span className={styles.popoutTitle}>{t`Voice Connection`}</span>
				<FocusRing offset={-2}>
					<button
						type="button"
						className={styles.popoutCloseButton}
						onClick={() => PopoutActionCreators.close()}
						aria-label={t`Close`}
					>
						<XIcon weight="bold" className={styles.iconSmall} />
					</button>
				</FocusRing>
			</div>

			{chartData.length > 0 && (
				<div className={styles.chartContainer}>
					<svg
						viewBox={`0 0 ${chartWidth} ${chartHeight}`}
						className={styles.chartSvg}
						style={{width: '100%'}}
						role="img"
						aria-label={t`Latency graph`}
					>
						{Array.from({length: 5}, (_, i) => Math.round((maxLatency / 4) * i)).map((value) => {
							const y = padding.top + graphHeight - (value / maxLatency) * graphHeight;
							return (
								<g key={value}>
									<line
										x1={padding.left}
										y1={y}
										x2={chartWidth - padding.right}
										y2={y}
										className={`${styles.gridLine} ${styles.gridLineHorizontal} ${styles.textBackgroundModifierHover}`}
									/>
									<text x={padding.left - 5} y={y} className={styles.gridText}>
										{value}ms
									</text>
								</g>
							);
						})}

						<line
							x1={padding.left}
							y1={chartHeight - padding.bottom}
							x2={chartWidth - padding.right}
							y2={chartHeight - padding.bottom}
							className={`${styles.gridLine} ${styles.textBackgroundModifierHover}`}
						/>

						<line
							x1={padding.left}
							y1={padding.top}
							x2={padding.left}
							y2={chartHeight - padding.bottom}
							className={`${styles.gridLine} ${styles.textBackgroundModifierHover}`}
						/>

						<path d={createLinePath()} className={`${styles.chartLine} ${styles.textGreen}`} />

						{chartData.map((point: LatencyDataPoint, index: number) => {
							const x = padding.left + (index / Math.max(chartData.length - 1, 1)) * graphWidth;
							const y = padding.top + graphHeight - (point.latency / maxLatency) * graphHeight;
							return (
								<circle
									key={point.timestamp}
									cx={x}
									cy={y}
									r="2"
									className={`${styles.chartPoint} ${styles.textGreen}`}
								/>
							);
						})}
					</svg>
				</div>
			)}

			<div className={styles.popoutStats}>
				{connectionId && (
					<div className={styles.popoutStatRow}>
						<span className={styles.popoutStatLabel}>{t`Device:`}</span>
						<Tooltip text={connectionId}>
							<div className={styles.deviceBadge}>
								{isMobile ? (
									<DeviceMobileIcon weight="regular" className={styles.deviceIcon} />
								) : (
									<DesktopIcon weight="regular" className={styles.deviceIcon} />
								)}
								<span className={styles.deviceBadgeText}>{connectionId}</span>
							</div>
						</Tooltip>
					</div>
				)}
				<div className={styles.popoutStatRow}>
					<span className={styles.popoutStatLabel}>{t`Current ping:`}</span>
					<span className={styles.popoutStatValue}>{latency !== null ? `${latency}ms` : t`Measuring...`}</span>
				</div>
				{averageLatency !== null && averageLatency !== undefined && (
					<div className={styles.popoutStatRow}>
						<span className={styles.popoutStatLabel}>{t`Average ping:`}</span>
						<span className={styles.popoutStatValue}>{averageLatency}ms</span>
					</div>
				)}
				{strippedEndpoint && (
					<div className={styles.popoutStatRow}>
						<span className={styles.popoutStatLabel}>{t`Endpoint:`}</span>
						<Tooltip text={strippedEndpoint}>
							<FocusRing offset={-2}>
								<div
									className={styles.endpointBadge}
									role="button"
									tabIndex={0}
									aria-label={t`Copy endpoint`}
									onClick={async (e) => {
										e.stopPropagation();
										await TextCopyActionCreators.copy(i18n, strippedEndpoint);
									}}
									onKeyDown={async (e) => {
										if (e.key === 'Enter' || e.key === ' ') {
											e.preventDefault();
											e.stopPropagation();
											await TextCopyActionCreators.copy(i18n, strippedEndpoint);
										}
									}}
								>
									<LockSimpleIcon weight="fill" className={styles.lockIcon} style={{color: 'var(--status-online)'}} />
									<span className={styles.endpointBadgeText}>{strippedEndpoint}</span>
								</div>
							</FocusRing>
						</Tooltip>
					</div>
				)}
			</div>
		</div>
	);
});

const VoiceConnectionStatusInner = observer(() => {
	const {t} = useLingui();
	const voiceState = MediaEngineStore.getCurrentUserVoiceState();
	const storeConnectedGuildId = MediaEngineStore.guildId;
	const storeConnectedChannelId = MediaEngineStore.channelId;
	const isConnecting = MediaEngineStore.connecting;
	const storeIsConnected = MediaEngineStore.connected;
	const voiceSettings = VoiceSettingsStore;
	const noiseSuppressionEnabled = voiceSettings.noiseSuppression;

	const currentLatency = MediaEngineStore.currentLatency;
	const latencyForSignal = currentLatency;
	const connectionId = MediaEngineStore.connectionId;
	const isMobile = voiceState?.is_mobile ?? false;

	const {openProps: popoutProps} = usePopout('voice-details-popout');

	const connectedGuildId = storeConnectedGuildId;
	const connectedChannelId = storeConnectedChannelId;
	const isConnected = storeIsConnected;

	if (!connectedGuildId || !connectedChannelId) {
		return null;
	}

	const channel = ChannelStore.getChannel(connectedChannelId);
	const guild = GuildStore.getGuild(connectedGuildId);

	if (!channel || !guild) {
		return null;
	}

	const getStatusText = () => {
		if (isConnecting) return t`Connecting...`;
		if (isConnected) return t`Voice Connected`;
		return t`Disconnected`;
	};

	const getStatusClass = () => {
		if (isConnecting) return styles.statusConnecting;
		if (isConnected) return styles.statusConnected;
		return styles.statusDisconnected;
	};

	return (
		<div className={styles.voiceConnectionContainer}>
			<div className={styles.statusRow}>
				{isConnected && (
					<Tooltip text={currentLatency !== null ? t`Ping: ${currentLatency}ms` : t`Measuring latency...`}>
						<div className={styles.signalIcon}>
							<SignalStrengthIcon latency={latencyForSignal} size={16} />
						</div>
					</Tooltip>
				)}
				<Popout {...popoutProps} position="top" offsetMainAxis={16} render={() => <VoiceDetailsPopout />}>
					<FocusRingWrapper focusRingOffset={-2}>
						<button type="button" className={clsx(styles.statusButton, getStatusClass())}>
							{getStatusText()}
						</button>
					</FocusRingWrapper>
				</Popout>
				<div className={styles.controls}>
					<Tooltip text={noiseSuppressionEnabled ? t`Disable Noise Suppression` : t`Enable Noise Suppression`}>
						<FocusRing offset={-2}>
							<button
								type="button"
								className={clsx(styles.controlButton, noiseSuppressionEnabled && styles.selected)}
								onClick={() => VoiceSettingsActionCreators.update({noiseSuppression: !noiseSuppressionEnabled})}
								aria-label={noiseSuppressionEnabled ? t`Disable Noise Suppression` : t`Enable Noise Suppression`}
							>
								<WaveformIcon weight="fill" className={styles.icon} />
							</button>
						</FocusRing>
					</Tooltip>
					<Tooltip text={t`Disconnect`}>
						<FocusRing offset={-2}>
							<button
								type="button"
								className={styles.controlButton}
								onClick={async () => {
									await MediaEngineStore.disconnectFromVoiceChannel();
								}}
								aria-label={t`Disconnect`}
							>
								<PhoneXIcon weight="fill" className={styles.icon} />
							</button>
						</FocusRing>
					</Tooltip>
				</div>
			</div>

			<div className={styles.connectionInfo}>
				{connectionId && (
					<div className={styles.infoRow}>
						{isMobile ? (
							<DeviceMobileIcon weight="regular" className={styles.infoIcon} />
						) : (
							<DesktopIcon weight="regular" className={styles.infoIcon} />
						)}
						<div>
							<Tooltip text={connectionId}>
								<span className={styles.infoText}>{connectionId}</span>
							</Tooltip>
						</div>
					</div>
				)}
				<FocusRing offset={-2}>
					<Link to={Routes.guildChannel(guild.id, channel.id)} className={styles.channelInfo}>
						<div className={styles.channelIcon}>
							{ChannelUtils.getIcon(channel, {className: styles.channelIconSize})}
						</div>
						<div className={styles.channelText}>
							<span className={styles.channelName}>{channel.name}</span>
							<span className={styles.guildSeparator}> / </span>
							<span className={styles.guildName}>{guild.name}</span>
						</div>
					</Link>
				</FocusRing>
			</div>

			<div className={styles.mediaSection}>
				<LocalParticipantControls />
			</div>
		</div>
	);
});

const LocalParticipantControls = observer(() => {
	const {t} = useLingui();
	const room = MediaEngineStore.room;
	const localParticipant = room?.localParticipant;
	const participants = MediaEngineStore.participants;
	const localParticipantSnapshot = Object.values(participants).find((p) => p.isLocal);
	const isCameraEnabled = localParticipantSnapshot?.isCameraEnabled ?? false;
	const isScreenShareEnabled = localParticipantSnapshot?.isScreenShareEnabled ?? false;
	const isConnected = !!room && !!localParticipant;

	const handleToggleCamera = useCallback(async () => {
		if (!localParticipant) return;

		try {
			if (isCameraEnabled) {
				await MediaEngineStore.setCameraEnabled(false);
			} else {
				ModalActionCreators.push(
					modal(() => (
						<CameraPreviewModalInRoom
							onEnabled={async () => {
								await MediaEngineStore.setCameraEnabled(true, {
									deviceId: VoiceSettingsStore.getVideoDeviceId() || undefined,
								});
							}}
						/>
					)),
				);
			}
		} catch (error) {
			console.error('Failed to toggle camera:', error);
		}
	}, [isCameraEnabled, localParticipant]);

	const getScreenShareConstraints = useCallback(
		(_resolution: 'low' | 'medium' | 'high' | 'ultra' | '4k', _frameRate: number) => {
			return {
				audio: true,
				selfBrowserSurface: 'include' as const,
				video: true as const,
			};
		},
		[],
	);

	const handleScreenShare = useCallback(async () => {
		if (!localParticipant) return;

		try {
			if (isScreenShareEnabled) {
				await MediaEngineStore.setScreenShareEnabled(false);
			} else {
				ModalActionCreators.push(
					modal(() => (
						<ScreenShareSettingsModal
							onStartShare={async (resolution, frameRate, includeAudio) => {
								await executeScreenShareOperation(async () => {
									const constraints = getScreenShareConstraints(resolution, frameRate);
									await MediaEngineStore.setScreenShareEnabled(true, {
										...constraints,
										audio: includeAudio,
									});
								});
							}}
						/>
					)),
				);
			}
		} catch (error) {
			console.error('Failed to toggle screen share:', error);
		}
	}, [isScreenShareEnabled, localParticipant, getScreenShareConstraints]);

	return (
		<>
			<Tooltip
				text={
					!isConnected ? t`Please wait for connection...` : isCameraEnabled ? t`Turn Off Camera` : t`Turn On Camera`
				}
			>
				<FocusRing offset={-2} enabled={isConnected}>
					<button
						type="button"
						className={clsx(styles.mediaButton, isCameraEnabled && styles.cameraActive)}
						onClick={handleToggleCamera}
						disabled={!isConnected}
						aria-label={
							!isConnected ? t`Please wait for connection...` : isCameraEnabled ? t`Turn Off Camera` : t`Turn On Camera`
						}
					>
						{isCameraEnabled ? (
							<CameraIcon weight="fill" className={styles.mediaIcon} />
						) : (
							<CameraSlashIcon weight="fill" className={styles.mediaIcon} />
						)}
					</button>
				</FocusRing>
			</Tooltip>
			<Tooltip
				text={
					!isConnected
						? t`Please wait for connection...`
						: isScreenShareEnabled
							? t`Stop Sharing`
							: t`Share Your Screen`
				}
			>
				<FocusRing offset={-2} enabled={isConnected}>
					<button
						type="button"
						className={clsx(styles.mediaButton, isScreenShareEnabled && styles.screenShareActive)}
						onClick={handleScreenShare}
						disabled={!isConnected}
						aria-label={
							!isConnected
								? t`Please wait for connection...`
								: isScreenShareEnabled
									? t`Stop Sharing`
									: t`Share Your Screen`
						}
					>
						<MonitorIcon weight="fill" className={styles.mediaIcon} />
					</button>
				</FocusRing>
			</Tooltip>
		</>
	);
});

const MockedVoiceConnectionStatus = observer(() => {
	const {i18n} = useLingui();
	const voiceSettings = VoiceSettingsStore;
	const noiseSuppressionEnabled = voiceSettings.noiseSuppression;
	const {openProps: popoutProps} = usePopout('voice-details-popout');
	const latency = 42;
	const averageLatency = 45;

	const generateMockLatencyData = () => {
		const data: Array<{timestamp: number; latency: number}> = [];
		const baseLatency = 45;
		for (let i = 0; i < 30; i++) {
			const variation = Math.sin(i / 3) * 15 + Math.random() * 10 - 5;
			data.push({
				timestamp: Date.now() - (30 - i) * 1000,
				latency: Math.max(20, Math.min(80, baseLatency + variation)),
			});
		}
		return data;
	};

	const chartData = generateMockLatencyData();
	const maxLatency = Math.max(...chartData.map((d) => d.latency), 0) + 10;

	return (
		<div className={styles.voiceConnectionContainer}>
			<div className={styles.statusRow}>
				<Tooltip text={t`Ping: ${latency}ms`}>
					<div className={styles.signalIcon}>
						<SignalStrengthIcon latency={latency} size={16} />
					</div>
				</Tooltip>
				<Popout
					{...popoutProps}
					position="top"
					offsetMainAxis={16}
					render={() => (
						<div className={styles.popoutContainer}>
							<div className={styles.popoutHeader}>
								<span className={styles.popoutTitle}>{t`Voice Connection`}</span>
								<FocusRing offset={-2}>
									<button
										type="button"
										className={styles.popoutCloseButton}
										onClick={() => PopoutActionCreators.close()}
										aria-label={t`Close`}
									>
										<XIcon weight="bold" className={styles.iconSmall} />
									</button>
								</FocusRing>
							</div>

							{chartData.length > 0 && (
								<div className={styles.chartContainer}>
									<svg
										viewBox="0 0 300 120"
										className={styles.chartSvg}
										style={{width: '100%'}}
										role="img"
										aria-label={t`Latency graph`}
									>
										{Array.from({length: 5}, (_, i) => Math.round((maxLatency / 4) * i)).map((value) => {
											const y = 110 - (value / maxLatency) * 80;
											return (
												<g key={value}>
													<line
														x1={40}
														y1={y}
														x2={290}
														y2={y}
														className={`${styles.gridLine} ${styles.gridLineHorizontal} ${styles.textBackgroundModifierHover}`}
													/>
													<text x={35} y={y} className={styles.gridText}>
														{value}ms
													</text>
												</g>
											);
										})}

										<line
											x1={40}
											y1={100}
											x2={290}
											y2={100}
											className={`${styles.gridLine} ${styles.textBackgroundModifierHover}`}
										/>

										<line
											x1={40}
											y1={20}
											x2={40}
											y2={100}
											className={`${styles.gridLine} ${styles.textBackgroundModifierHover}`}
										/>

										<path
											d={(() => {
												if (chartData.length === 0) return '';
												const points = chartData.map((point, index) => {
													const x = 40 + (index / Math.max(chartData.length - 1, 1)) * 250;
													const y = 110 - (point.latency / maxLatency) * 80;
													return `${x},${y}`;
												});
												return `M ${points.join(' L ')}`;
											})()}
											className={`${styles.chartLine} ${styles.textGreen}`}
										/>

										{chartData.map((point, index) => {
											const x = 40 + (index / Math.max(chartData.length - 1, 1)) * 250;
											const y = 110 - (point.latency / maxLatency) * 80;
											return (
												<circle
													key={point.timestamp}
													cx={x}
													cy={y}
													r="2"
													className={`${styles.chartPoint} ${styles.textGreen}`}
												/>
											);
										})}
									</svg>
								</div>
							)}

							<div className={styles.popoutStats}>
								<div className={styles.popoutStatRow}>
									<span className={styles.popoutStatLabel}>{t`Device:`}</span>
									<Tooltip text="mock-device-1">
										<div className={styles.deviceBadge}>
											<DesktopIcon weight="regular" className={styles.deviceIcon} />
											<span className={styles.deviceBadgeText}>mock-device-1</span>
										</div>
									</Tooltip>
								</div>
								<div className={styles.popoutStatRow}>
									<span className={styles.popoutStatLabel}>{t`Current ping:`}</span>
									<span className={styles.popoutStatValue}>{latency}ms</span>
								</div>
								<div className={styles.popoutStatRow}>
									<span className={styles.popoutStatLabel}>{t`Average ping:`}</span>
									<span className={styles.popoutStatValue}>{averageLatency}ms</span>
								</div>
								<div className={styles.popoutStatRow}>
									<span className={styles.popoutStatLabel}>{t`Endpoint:`}</span>
									<Tooltip text="mock.voice.server:443">
										<FocusRing offset={-2}>
											<div
												className={styles.endpointBadge}
												role="button"
												tabIndex={0}
												aria-label={t`Copy endpoint`}
												onClick={async (e) => {
													e.stopPropagation();
													await TextCopyActionCreators.copy(i18n, 'mock.voice.server:443');
												}}
												onKeyDown={async (e) => {
													if (e.key === 'Enter' || e.key === ' ') {
														e.preventDefault();
														e.stopPropagation();
														await TextCopyActionCreators.copy(i18n, 'mock.voice.server:443');
													}
												}}
											>
												<LockSimpleIcon
													weight="fill"
													className={styles.lockIcon}
													style={{color: 'var(--status-online)'}}
												/>
												<span className={styles.endpointBadgeText}>mock.voice.server:443</span>
											</div>
										</FocusRing>
									</Tooltip>
								</div>
							</div>
						</div>
					)}
				>
					<FocusRingWrapper focusRingOffset={-2}>
						<button type="button" className={clsx(styles.statusButton, styles.statusConnected)}>
							{t`Voice Connected`}
						</button>
					</FocusRingWrapper>
				</Popout>
				<div className={styles.controls}>
					<Tooltip text={noiseSuppressionEnabled ? t`Disable Noise Suppression` : t`Enable Noise Suppression`}>
						<FocusRing offset={-2}>
							<button
								type="button"
								className={clsx(styles.controlButton, noiseSuppressionEnabled && styles.selected)}
								onClick={() => VoiceSettingsActionCreators.update({noiseSuppression: !noiseSuppressionEnabled})}
								aria-label={noiseSuppressionEnabled ? t`Disable Noise Suppression` : t`Enable Noise Suppression`}
							>
								<WaveformIcon weight="fill" className={styles.icon} />
							</button>
						</FocusRing>
					</Tooltip>
					<Tooltip text={t`Disconnect`}>
						<FocusRing offset={-2}>
							<button
								type="button"
								className={styles.controlButton}
								onClick={() => {
									DeveloperOptionsActionCreators.updateOption('forceShowVoiceConnection', false);
								}}
								aria-label={t`Disconnect`}
							>
								<PhoneXIcon weight="fill" className={styles.icon} />
							</button>
						</FocusRing>
					</Tooltip>
				</div>
			</div>

			<div className={styles.connectionInfo}>
				<div className={styles.infoRow}>
					<DesktopIcon weight="regular" className={styles.infoIcon} />
					<div>
						<Tooltip text="mock-device-1">
							<span className={styles.infoText}>mock-device-1</span>
						</Tooltip>
					</div>
				</div>
				<div className={styles.channelInfo}>
					<div className={styles.channelIcon}>
						{ChannelUtils.getIcon({type: 2}, {className: styles.channelIconSize})}
					</div>
					<div className={styles.channelText}>
						<span className={styles.channelName}>general</span>
						<span className={styles.guildSeparator}> / </span>
						<span className={styles.guildName}>Mock Guild</span>
					</div>
				</div>
			</div>

			<div className={styles.mediaSection}>
				<Tooltip text={t`Turn On Camera`}>
					<FocusRing offset={-2}>
						<button type="button" className={styles.mediaButton} aria-label={t`Turn On Camera`}>
							<CameraSlashIcon weight="fill" className={styles.mediaIcon} />
						</button>
					</FocusRing>
				</Tooltip>
				<Tooltip text={t`Share Your Screen`}>
					<FocusRing offset={-2}>
						<button type="button" className={styles.mediaButton} aria-label={t`Share Your Screen`}>
							<MonitorIcon weight="fill" className={styles.mediaIcon} />
						</button>
					</FocusRing>
				</Tooltip>
			</div>
		</div>
	);
});

export const VoiceConnectionStatus = observer(() => {
	const storeConnectedGuildId = MediaEngineStore.guildId;
	const storeConnectedChannelId = MediaEngineStore.channelId;
	const mobileLayout = MobileLayoutStore;
	const forceShowVoiceConnection = DeveloperOptionsStore.forceShowVoiceConnection;

	if (mobileLayout.enabled) {
		return null;
	}

	const connectedGuildId = storeConnectedGuildId;
	const connectedChannelId = storeConnectedChannelId;

	if (!connectedGuildId || !connectedChannelId) {
		if (!forceShowVoiceConnection) {
			return null;
		}
		return <MockedVoiceConnectionStatus />;
	}

	return <VoiceConnectionStatusInner />;
});
