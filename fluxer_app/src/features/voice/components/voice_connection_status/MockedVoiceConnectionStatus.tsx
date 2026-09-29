// SPDX-License-Identifier: AGPL-3.0-or-later

import * as DeveloperOptionsCommands from '@app/features/devtools/commands/DeveloperOptionsCommands';
import {CLOSE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as PopoutCommands from '@app/features/ui/commands/PopoutCommands';
import {FocusRingWrapper} from '@app/features/ui/components/FocusRingWrapper';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {usePopout} from '@app/features/ui/hooks/usePopout';
import {Popout} from '@app/features/ui/popover/PopoverPopout';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {SignalStrengthIcon} from '@app/features/voice/components/SignalStrengthIcon';
import styles from '@app/features/voice/components/VoiceConnectionStatus.module.css';
import {AudioProcessingButton} from '@app/features/voice/components/voice_connection_status/AudioProcessingButton';
import {AudioProcessingModal} from '@app/features/voice/components/voice_connection_status/AudioProcessingModal';
import {EndpointCopyBadge} from '@app/features/voice/components/voice_connection_status/EndpointCopyBadge';
import {
	AVERAGE_PING_DESCRIPTOR,
	COPY_ENDPOINT_DESCRIPTOR,
	CURRENT_PING_DESCRIPTOR,
	DEVICE_DESCRIPTOR,
	ENDPOINT_DESCRIPTOR,
	getAudioProcessingTooltip,
	isAudioProcessingActive,
	LATENCY_GRAPH_DESCRIPTOR,
} from '@app/features/voice/components/voice_connection_status/shared';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {readNoiseSuppressionBackend} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionRuntime';
import {
	VOICE_DISCONNECT_DESCRIPTOR,
	VOICE_IN_CHAT_DESCRIPTOR,
	VOICE_SHARE_SCREEN_DESCRIPTOR,
	VOICE_TURN_ON_CAMERA_DESCRIPTOR,
} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {getActiveVoiceProcessingMode} from '@app/features/voice/utils/VoiceProcessingProfile';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {CameraSlashIcon, DesktopIcon, MonitorPlayIcon, PhoneXIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {useCallback} from 'react';

const PING_MS_2_DESCRIPTOR = msg({
	message: 'Ping: {latency}ms',
	comment: 'Compact latency badge variant used in another voice surface. {latency} is an integer millisecond count.',
});
const VOICE_CONNECTION_2_DESCRIPTOR = msg({
	message: 'Voice connection',
	comment: 'Voice status popout title (secondary call site for the same surface as VOICE_CONNECTION_DESCRIPTOR).',
});
export const MockedVoiceConnectionStatus = observer(() => {
	const {i18n} = useLingui();
	const voiceSettings = VoiceSettings;
	const processingMode = getActiveVoiceProcessingMode(voiceSettings);
	const noiseSuppressionBackend = readNoiseSuppressionBackend();
	const isProcessingActive = isAudioProcessingActive(processingMode, noiseSuppressionBackend);
	const showVoiceConnectionId = voiceSettings.showVoiceConnectionId;
	const openNoiseSuppressionModal = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<AudioProcessingModal data-flx="voice.voice-connection-status.open-noise-suppression-modal.audio-processing-modal--2" />
			)),
		);
	}, []);
	const noiseSuppressionTooltip = getAudioProcessingTooltip(i18n, processingMode, noiseSuppressionBackend);
	const disconnectLabel = i18n._(VOICE_DISCONNECT_DESCRIPTOR);
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
		<div
			className={styles.voiceConnectionContainer}
			data-flx="voice.voice-connection-status.mocked-voice-connection-status.voice-connection-container"
		>
			<div
				className={styles.statusRow}
				data-flx="voice.voice-connection-status.mocked-voice-connection-status.status-row"
			>
				<Tooltip
					text={i18n._(PING_MS_2_DESCRIPTOR, {latency})}
					data-flx="voice.voice-connection-status.mocked-voice-connection-status.tooltip"
				>
					<div
						className={styles.signalIcon}
						data-flx="voice.voice-connection-status.mocked-voice-connection-status.signal-icon"
					>
						<SignalStrengthIcon
							latency={latency}
							size={16}
							data-flx="voice.voice-connection-status.mocked-voice-connection-status.signal-strength-icon"
						/>
					</div>
				</Tooltip>
				<Popout
					data-flx="voice.voice-connection-status.mocked-voice-connection-status.popout"
					{...popoutProps}
					position="top"
					offsetMainAxis={16}
					render={() => (
						<div
							className={styles.popoutContainer}
							data-flx="voice.voice-connection-status.mocked-voice-connection-status.popout-container"
						>
							<div
								className={styles.popoutHeader}
								data-flx="voice.voice-connection-status.mocked-voice-connection-status.popout-header"
							>
								<span
									className={styles.popoutTitle}
									data-flx="voice.voice-connection-status.mocked-voice-connection-status.popout-title"
								>
									{i18n._(VOICE_CONNECTION_2_DESCRIPTOR)}
								</span>
								<FocusRing
									offset={-2}
									data-flx="voice.voice-connection-status.mocked-voice-connection-status.focus-ring"
								>
									<button
										type="button"
										className={styles.popoutCloseButton}
										onClick={() => PopoutCommands.close()}
										aria-label={i18n._(CLOSE_DESCRIPTOR)}
										data-flx="voice.voice-connection-status.mocked-voice-connection-status.popout-close-button"
									>
										<XIcon
											weight="bold"
											className={styles.iconSmall}
											data-flx="voice.voice-connection-status.mocked-voice-connection-status.icon-small"
										/>
									</button>
								</FocusRing>
							</div>
							{chartData.length > 0 && (
								<div
									className={styles.chartContainer}
									data-flx="voice.voice-connection-status.mocked-voice-connection-status.chart-container"
								>
									<svg
										viewBox="0 0 300 120"
										className={styles.chartSvg}
										role="img"
										aria-label={i18n._(LATENCY_GRAPH_DESCRIPTOR)}
										data-flx="voice.voice-connection-status.mocked-voice-connection-status.chart-svg"
									>
										{Array.from({length: 5}, (_, i) => Math.round((maxLatency / 4) * i)).map((value) => {
											const y = 110 - (value / maxLatency) * 80;
											return (
												<g key={value} data-flx="voice.voice-connection-status.mocked-voice-connection-status.g">
													<line
														x1={40}
														y1={y}
														x2={290}
														y2={y}
														className={`${styles.gridLine} ${styles.gridLineHorizontal} ${styles.textBackgroundModifierHover}`}
														data-flx="voice.voice-connection-status.mocked-voice-connection-status.grid-line"
													/>
													<text
														x={35}
														y={y}
														className={styles.gridText}
														data-flx="voice.voice-connection-status.mocked-voice-connection-status.grid-text"
													>
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
											data-flx="voice.voice-connection-status.mocked-voice-connection-status.grid-line--2"
										/>
										<line
											x1={40}
											y1={20}
											x2={40}
											y2={100}
											className={`${styles.gridLine} ${styles.textBackgroundModifierHover}`}
											data-flx="voice.voice-connection-status.mocked-voice-connection-status.grid-line--3"
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
											data-flx="voice.voice-connection-status.mocked-voice-connection-status.chart-line"
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
													data-flx="voice.voice-connection-status.mocked-voice-connection-status.chart-point"
												/>
											);
										})}
									</svg>
								</div>
							)}
							<div
								className={styles.popoutStats}
								data-flx="voice.voice-connection-status.mocked-voice-connection-status.popout-stats"
							>
								<div
									className={styles.popoutStatRow}
									data-flx="voice.voice-connection-status.mocked-voice-connection-status.popout-stat-row"
								>
									<span
										className={styles.popoutStatLabel}
										data-flx="voice.voice-connection-status.mocked-voice-connection-status.popout-stat-label"
									>
										{i18n._(DEVICE_DESCRIPTOR)}
									</span>
									<Tooltip
										text="mock-device-1"
										data-flx="voice.voice-connection-status.mocked-voice-connection-status.tooltip--2"
									>
										<div
											className={styles.deviceBadge}
											data-flx="voice.voice-connection-status.mocked-voice-connection-status.device-badge"
										>
											<DesktopIcon
												weight="regular"
												className={styles.deviceIcon}
												data-flx="voice.voice-connection-status.mocked-voice-connection-status.device-icon"
											/>
											<span
												className={styles.deviceBadgeText}
												data-flx="voice.voice-connection-status.mocked-voice-connection-status.device-badge-text"
											>
												mock-device-1
											</span>
										</div>
									</Tooltip>
								</div>
								<div
									className={styles.popoutStatRow}
									data-flx="voice.voice-connection-status.mocked-voice-connection-status.popout-stat-row--2"
								>
									<span
										className={styles.popoutStatLabel}
										data-flx="voice.voice-connection-status.mocked-voice-connection-status.popout-stat-label--2"
									>
										{i18n._(CURRENT_PING_DESCRIPTOR)}
									</span>
									<span
										className={styles.popoutStatValue}
										data-flx="voice.voice-connection-status.mocked-voice-connection-status.popout-stat-value"
									>
										{latency}ms
									</span>
								</div>
								<div
									className={styles.popoutStatRow}
									data-flx="voice.voice-connection-status.mocked-voice-connection-status.popout-stat-row--3"
								>
									<span
										className={styles.popoutStatLabel}
										data-flx="voice.voice-connection-status.mocked-voice-connection-status.popout-stat-label--3"
									>
										{i18n._(AVERAGE_PING_DESCRIPTOR)}
									</span>
									<span
										className={styles.popoutStatValue}
										data-flx="voice.voice-connection-status.mocked-voice-connection-status.popout-stat-value--2"
									>
										{averageLatency}ms
									</span>
								</div>
								<div
									className={styles.popoutStatRow}
									data-flx="voice.voice-connection-status.mocked-voice-connection-status.popout-stat-row--4"
								>
									<span
										className={styles.popoutStatLabel}
										data-flx="voice.voice-connection-status.mocked-voice-connection-status.popout-stat-label--4"
									>
										{i18n._(ENDPOINT_DESCRIPTOR)}
									</span>
									<EndpointCopyBadge
										endpoint="mock.voice.server:443"
										i18n={i18n}
										label={i18n._(COPY_ENDPOINT_DESCRIPTOR)}
										data-flx="voice.voice-connection-status.mocked-voice-connection-status.endpoint-copy-badge"
									/>
								</div>
							</div>
						</div>
					)}
				>
					<FocusRingWrapper
						focusRingOffset={-2}
						data-flx="voice.voice-connection-status.mocked-voice-connection-status.focus-ring-wrapper"
					>
						<button
							type="button"
							className={clsx(styles.statusButton, styles.statusConnected)}
							data-flx="voice.voice-connection-status.mocked-voice-connection-status.status-button"
						>
							{i18n._(VOICE_IN_CHAT_DESCRIPTOR)}
						</button>
					</FocusRingWrapper>
				</Popout>
				<div
					className={styles.controls}
					data-flx="voice.voice-connection-status.mocked-voice-connection-status.controls"
				>
					<AudioProcessingButton
						active={isProcessingActive}
						label={noiseSuppressionTooltip}
						onClick={openNoiseSuppressionModal}
						data-flx="voice.voice-connection-status.mocked-voice-connection-status.audio-processing-button.open-noise-suppression-modal"
					/>
					<Tooltip
						text={disconnectLabel}
						data-flx="voice.voice-connection-status.mocked-voice-connection-status.tooltip--3"
					>
						<FocusRing
							offset={-2}
							data-flx="voice.voice-connection-status.mocked-voice-connection-status.focus-ring--2"
						>
							<button
								type="button"
								className={styles.controlButton}
								onClick={() => {
									DeveloperOptionsCommands.updateOption('forceShowVoiceConnection', false);
								}}
								aria-label={disconnectLabel}
								data-flx="voice.voice-connection-status.mocked-voice-connection-status.control-button.update-option"
							>
								<PhoneXIcon
									weight="fill"
									className={styles.icon}
									data-flx="voice.voice-connection-status.mocked-voice-connection-status.icon"
								/>
							</button>
						</FocusRing>
					</Tooltip>
				</div>
			</div>
			<div
				className={styles.connectionInfo}
				data-flx="voice.voice-connection-status.mocked-voice-connection-status.connection-info"
			>
				<div
					className={styles.channelSourceRow}
					data-flx="voice.voice-connection-status.mocked-voice-connection-status.channel-source-row"
				>
					<FocusRing offset={-2} data-flx="voice.voice-connection-status.mocked-voice-connection-status.focus-ring--3">
						<button
							type="button"
							className={styles.channelSourceLink}
							data-flx="voice.voice-connection-status.mocked-voice-connection-status.channel-source-link.button"
						>
							<span
								className={styles.channelSourceText}
								data-flx="voice.voice-connection-status.mocked-voice-connection-status.channel-source-text"
							>
								<span
									className={styles.channelSourceChannel}
									data-flx="voice.voice-connection-status.mocked-voice-connection-status.channel-source-channel"
								>
									general
								</span>
								<span
									className={styles.channelSourceSeparator}
									data-flx="voice.voice-connection-status.mocked-voice-connection-status.channel-source-separator"
								>
									{' '}
									/{' '}
								</span>
								<span
									className={styles.channelSourceGuild}
									data-flx="voice.voice-connection-status.mocked-voice-connection-status.channel-source-guild"
								>
									Mock Guild
								</span>
							</span>
						</button>
					</FocusRing>
				</div>
				{showVoiceConnectionId && (
					<div
						className={styles.connectionIdRow}
						data-flx="voice.voice-connection-status.mocked-voice-connection-status.connection-id-row"
					>
						<DesktopIcon
							weight="regular"
							className={styles.connectionIdIcon}
							data-flx="voice.voice-connection-status.mocked-voice-connection-status.connection-id-icon"
						/>
						<div
							className={styles.connectionIdValue}
							data-flx="voice.voice-connection-status.mocked-voice-connection-status.connection-id-value"
						>
							<Tooltip
								text="mock-connection-1"
								position="top"
								align="center"
								data-flx="voice.voice-connection-status.mocked-voice-connection-status.tooltip--4"
							>
								<span
									className={styles.connectionIdValueText}
									data-flx="voice.voice-connection-status.mocked-voice-connection-status.connection-id-value-text"
								>
									mock-connection-1
								</span>
							</Tooltip>
						</div>
					</div>
				)}
			</div>
			<div
				className={styles.mediaSection}
				data-flx="voice.voice-connection-status.mocked-voice-connection-status.media-section"
			>
				<Tooltip
					text={i18n._(VOICE_TURN_ON_CAMERA_DESCRIPTOR)}
					data-flx="voice.voice-connection-status.mocked-voice-connection-status.tooltip--5"
				>
					<FocusRing offset={-2} data-flx="voice.voice-connection-status.mocked-voice-connection-status.focus-ring--4">
						<button
							type="button"
							className={styles.mediaButton}
							aria-label={i18n._(VOICE_TURN_ON_CAMERA_DESCRIPTOR)}
							data-flx="voice.voice-connection-status.mocked-voice-connection-status.media-button"
						>
							<CameraSlashIcon
								weight="fill"
								className={styles.mediaIcon}
								data-flx="voice.voice-connection-status.mocked-voice-connection-status.media-icon"
							/>
						</button>
					</FocusRing>
				</Tooltip>
				<Tooltip
					text={i18n._(VOICE_SHARE_SCREEN_DESCRIPTOR)}
					data-flx="voice.voice-connection-status.mocked-voice-connection-status.tooltip--6"
				>
					<FocusRing offset={-2} data-flx="voice.voice-connection-status.mocked-voice-connection-status.focus-ring--5">
						<button
							type="button"
							className={styles.mediaButton}
							aria-label={i18n._(VOICE_SHARE_SCREEN_DESCRIPTOR)}
							data-flx="voice.voice-connection-status.mocked-voice-connection-status.media-button--2"
						>
							<MonitorPlayIcon
								weight="fill"
								className={styles.mediaIcon}
								data-flx="voice.voice-connection-status.mocked-voice-connection-status.media-icon--2"
							/>
						</button>
					</FocusRing>
				</Tooltip>
			</div>
		</div>
	);
});
