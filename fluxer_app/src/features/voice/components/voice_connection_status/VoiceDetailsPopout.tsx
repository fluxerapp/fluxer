// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {CLOSE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {getCachedNumberFormat} from '@app/features/i18n/utils/IntlCache';
import * as PopoutCommands from '@app/features/ui/commands/PopoutCommands';
import {Scroller} from '@app/features/ui/components/Scroller';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import styles from '@app/features/voice/components/VoiceConnectionStatus.module.css';
import {EndpointCopyBadge} from '@app/features/voice/components/voice_connection_status/EndpointCopyBadge';
import {
	AUDIO_PACKET_LOSS_DESCRIPTOR,
	AUDIO_RECEIVE_BANDWIDTH_DESCRIPTOR,
	AUDIO_SEND_BANDWIDTH_DESCRIPTOR,
	AVERAGE_PING_DESCRIPTOR,
	BANDWIDTH_DESCRIPTOR,
	COPY_ENDPOINT_DESCRIPTOR,
	CURRENT_PING_DESCRIPTOR,
	DEVICE_DESCRIPTOR,
	DURATION_DESCRIPTOR,
	ENDPOINT_DESCRIPTOR,
	formatBitrateBps,
	formatBitrateKbps,
	formatMilliseconds,
	formatPacketLossPercent,
	INCOMING_CAPACITY_DESCRIPTOR,
	JITTER_DESCRIPTOR,
	LATENCY_GRAPH_DESCRIPTOR,
	MEASURING_DESCRIPTOR,
	NETWORK_DESCRIPTOR,
	OUTGOING_CAPACITY_DESCRIPTOR,
	PARTICIPANTS_DESCRIPTOR,
	PUBLISHER_TRANSPORT_DESCRIPTOR,
	RECEIVE_BANDWIDTH_DESCRIPTOR,
	SEND_BANDWIDTH_DESCRIPTOR,
	SESSION_DESCRIPTOR,
	SUBSCRIBER_TRANSPORT_DESCRIPTOR,
	VIDEO_PACKET_LOSS_DESCRIPTOR,
	VIDEO_RECEIVE_BANDWIDTH_DESCRIPTOR,
	VIDEO_SEND_BANDWIDTH_DESCRIPTOR,
} from '@app/features/voice/components/voice_connection_status/shared';
import MediaEngine, {useMediaEngineVersion} from '@app/features/voice/engine/MediaEngineFacade';
import {formatDuration} from '@fluxer/date_utils/src/DateDuration';
import type {
	VoiceEngineV2LatencyDataPoint as LatencyDataPoint,
	VoiceEngineV2TransportInfo as TransportInfo,
} from '@fluxer/voice_engine_v2';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {CaretDownIcon, DesktopIcon, DeviceMobileIcon, XIcon} from '@phosphor-icons/react';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import {type ReactNode, useId, useState} from 'react';

const VOICE_CONNECTION_DESCRIPTOR = msg({
	message: 'Voice connection',
	comment: 'Voice status popout title.',
});
const ADVANCED_STATS_DESCRIPTOR = msg({
	message: 'Advanced',
	comment:
		'Disclosure button label in the voice connection status popout. Reveals technical bandwidth and network stats.',
});
const ADVANCED_STATS_TRANSITION = {
	duration: 0.24,
	ease: 'easeOut' as const,
};
const INSTANT_TRANSITION = {duration: 0};

interface PopoutStatsSectionProps {
	title: string;
	children: ReactNode;
}

function PopoutStatsSection({title, children}: PopoutStatsSectionProps) {
	return (
		<div className={styles.popoutStatsSection} data-flx="voice.voice-connection-status.popout-stats-section">
			<div
				className={styles.popoutStatsSectionTitle}
				data-flx="voice.voice-connection-status.popout-stats-section-title"
			>
				{title}
			</div>
			<div className={styles.popoutStatsSectionRows} data-flx="voice.voice-connection-status.popout-stats-section-rows">
				{children}
			</div>
		</div>
	);
}

interface PopoutStatRowProps {
	label: string;
	value: ReactNode;
}

function PopoutStatRow({label, value}: PopoutStatRowProps) {
	return (
		<div className={styles.popoutStatRow} data-flx="voice.voice-connection-status.popout-stat-row">
			<span className={styles.popoutStatLabel} data-flx="voice.voice-connection-status.popout-stat-label">
				{label}
			</span>
			<div className={styles.popoutStatValue} data-flx="voice.voice-connection-status.popout-stat-value">
				{value}
			</div>
		</div>
	);
}

function formatTransportSummary(transport: TransportInfo | null): string | null {
	if (!transport) return null;
	const parts = [
		transport.localNetworkType,
		transport.localCandidateType,
		transport.localProtocol,
		transport.remoteCandidateType,
		transport.remoteProtocol,
		transport.candidatePairState,
		transport.iceState,
		transport.dtlsState,
	].filter((part): part is string => Boolean(part));
	return parts.length > 0 ? parts.join(' / ') : null;
}

interface AdvancedStatsAccordionProps {
	title: string;
	children: ReactNode;
}

const AdvancedStatsAccordion = observer(function AdvancedStatsAccordion({
	title,
	children,
}: AdvancedStatsAccordionProps) {
	const [expanded, setExpanded] = useState(false);
	const generatedId = useId();
	const contentId = `${generatedId}-advanced-stats-content`;
	const transition = Accessibility.useReducedMotion ? INSTANT_TRANSITION : ADVANCED_STATS_TRANSITION;
	return (
		<div
			className={styles.detailedStatsAccordion}
			data-flx="voice.voice-connection-status.advanced-stats-accordion.advanced-stats-accordion"
		>
			<FocusRing offset={-2} data-flx="voice.voice-connection-status.advanced-stats-accordion.focus-ring">
				<button
					type="button"
					className={styles.detailedStatsButton}
					aria-expanded={expanded}
					aria-controls={contentId}
					onClick={() => setExpanded((current) => !current)}
					data-flx="voice.voice-connection-status.advanced-stats-accordion.advanced-stats-button"
				>
					<span
						className={styles.detailedStatsButtonText}
						data-flx="voice.voice-connection-status.advanced-stats-accordion.advanced-stats-button-text"
					>
						{title}
					</span>
					<motion.span
						className={styles.detailedStatsCaret}
						aria-hidden
						animate={{rotate: expanded ? 180 : 0}}
						transition={transition}
						data-flx="voice.voice-connection-status.advanced-stats-accordion.advanced-stats-caret"
					>
						<CaretDownIcon
							weight="bold"
							className={styles.iconSmall}
							data-flx="voice.voice-connection-status.advanced-stats-accordion.icon-small"
						/>
					</motion.span>
				</button>
			</FocusRing>
			<AnimatePresence
				initial={false}
				data-flx="voice.voice-connection-status.advanced-stats-accordion.animate-presence"
			>
				{expanded && (
					<motion.div
						key="advanced-stats"
						id={contentId}
						className={styles.detailedStatsContent}
						initial={Accessibility.useReducedMotion ? {height: 'auto', opacity: 1} : {height: 0, opacity: 0}}
						animate={{height: 'auto', opacity: 1}}
						exit={Accessibility.useReducedMotion ? {height: 'auto', opacity: 1} : {height: 0, opacity: 0}}
						transition={transition}
						data-flx="voice.voice-connection-status.advanced-stats-accordion.advanced-stats-content"
					>
						<div
							className={styles.detailedStatsInner}
							data-flx="voice.voice-connection-status.advanced-stats-accordion.advanced-stats-inner"
						>
							{children}
						</div>
					</motion.div>
				)}
			</AnimatePresence>
		</div>
	);
});

interface VoiceDetailsPopoutProps {
	onClose?: () => void;
	hideHeader?: boolean;
}

export const VoiceDetailsPopout = observer(({onClose, hideHeader = false}: VoiceDetailsPopoutProps = {}) => {
	const {i18n} = useLingui();
	useMediaEngineVersion();
	const handleClose = () => {
		if (onClose) {
			onClose();
			return;
		}
		PopoutCommands.close();
	};
	const locale = i18n.locale;
	const latency = MediaEngine.currentLatency;
	const averageLatency = MediaEngine.averageLatency;
	const latencyHistory = MediaEngine.latencyHistory;
	const voiceServerEndpoint = MediaEngine.voiceServerEndpoint;
	const connectionId = MediaEngine.connectionId;
	const stats = MediaEngine.voiceStats;
	const publisherTransport = MediaEngine.publisherTransport;
	const subscriberTransport = MediaEngine.subscriberTransport;
	const voiceState = MediaEngine.getCurrentUserVoiceState();
	const isMobile = voiceState?.is_mobile ?? false;
	const totalSendBitrate = stats.audioSendBitrate + stats.videoSendBitrate;
	const totalReceiveBitrate = stats.audioRecvBitrate + stats.videoRecvBitrate;
	const outgoingCapacity = formatBitrateBps(
		publisherTransport?.availableOutgoingBitrate ?? subscriberTransport?.availableOutgoingBitrate,
		locale,
	);
	const incomingCapacity = formatBitrateBps(
		subscriberTransport?.availableIncomingBitrate ?? publisherTransport?.availableIncomingBitrate,
		locale,
	);
	const publisherTransportSummary = formatTransportSummary(publisherTransport);
	const subscriberTransportSummary = formatTransportSummary(subscriberTransport);
	const participantCount = getCachedNumberFormat(locale, {maximumFractionDigits: 0}).format(stats.participantCount);
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
		<div
			className={styles.popoutContainer}
			data-flx="voice.voice-connection-status.voice-details-popout.popout-container"
		>
			{!hideHeader && (
				<div
					className={styles.popoutHeader}
					data-flx="voice.voice-connection-status.voice-details-popout.popout-header"
				>
					<span
						className={styles.popoutTitle}
						data-flx="voice.voice-connection-status.voice-details-popout.popout-title"
					>
						{i18n._(VOICE_CONNECTION_DESCRIPTOR)}
					</span>
					<FocusRing offset={-2} data-flx="voice.voice-connection-status.voice-details-popout.focus-ring">
						<button
							type="button"
							className={styles.popoutCloseButton}
							onClick={handleClose}
							aria-label={i18n._(CLOSE_DESCRIPTOR)}
							data-flx="voice.voice-connection-status.voice-details-popout.popout-close-button"
						>
							<XIcon
								weight="bold"
								className={styles.iconSmall}
								data-flx="voice.voice-connection-status.voice-details-popout.icon-small"
							/>
						</button>
					</FocusRing>
				</div>
			)}
			<Scroller
				className={styles.popoutBodyScroller}
				contentClassName={styles.popoutBodyContent}
				overflow="auto"
				data-flx="voice.voice-connection-status.voice-details-popout.body-scroller"
			>
				{chartData.length > 0 && (
					<div
						className={styles.chartContainer}
						data-flx="voice.voice-connection-status.voice-details-popout.chart-container"
					>
						<svg
							viewBox={`0 0 ${chartWidth} ${chartHeight}`}
							className={styles.chartSvg}
							role="img"
							aria-label={i18n._(LATENCY_GRAPH_DESCRIPTOR)}
							data-flx="voice.voice-connection-status.voice-details-popout.chart-svg"
						>
							{Array.from({length: 5}, (_, i) => Math.round((maxLatency / 4) * i)).map((value) => {
								const y = padding.top + graphHeight - (value / maxLatency) * graphHeight;
								return (
									<g key={value} data-flx="voice.voice-connection-status.voice-details-popout.g">
										<line
											x1={padding.left}
											y1={y}
											x2={chartWidth - padding.right}
											y2={y}
											className={`${styles.gridLine} ${styles.gridLineHorizontal} ${styles.textBackgroundModifierHover}`}
											data-flx="voice.voice-connection-status.voice-details-popout.grid-line"
										/>
										<text
											x={padding.left - 5}
											y={y}
											className={styles.gridText}
											data-flx="voice.voice-connection-status.voice-details-popout.grid-text"
										>
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
								data-flx="voice.voice-connection-status.voice-details-popout.grid-line--2"
							/>
							<line
								x1={padding.left}
								y1={padding.top}
								x2={padding.left}
								y2={chartHeight - padding.bottom}
								className={`${styles.gridLine} ${styles.textBackgroundModifierHover}`}
								data-flx="voice.voice-connection-status.voice-details-popout.grid-line--3"
							/>
							<path
								d={createLinePath()}
								className={`${styles.chartLine} ${styles.textGreen}`}
								data-flx="voice.voice-connection-status.voice-details-popout.chart-line"
							/>
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
										data-flx="voice.voice-connection-status.voice-details-popout.chart-point"
									/>
								);
							})}
						</svg>
					</div>
				)}
				<div className={styles.popoutStats} data-flx="voice.voice-connection-status.voice-details-popout.popout-stats">
					<PopoutStatsSection
						title={i18n._(SESSION_DESCRIPTOR)}
						data-flx="voice.voice-connection-status.voice-details-popout.popout-stats-section"
					>
						{connectionId && (
							<PopoutStatRow
								label={i18n._(DEVICE_DESCRIPTOR)}
								value={
									<Tooltip text={connectionId} data-flx="voice.voice-connection-status.voice-details-popout.tooltip">
										<div
											className={styles.deviceBadge}
											data-flx="voice.voice-connection-status.voice-details-popout.device-badge"
										>
											{isMobile ? (
												<DeviceMobileIcon
													weight="regular"
													className={styles.deviceIcon}
													data-flx="voice.voice-connection-status.voice-details-popout.device-icon"
												/>
											) : (
												<DesktopIcon
													weight="regular"
													className={styles.deviceIcon}
													data-flx="voice.voice-connection-status.voice-details-popout.device-icon--2"
												/>
											)}
											<span
												className={styles.deviceBadgeText}
												data-flx="voice.voice-connection-status.voice-details-popout.device-badge-text"
											>
												{connectionId}
											</span>
										</div>
									</Tooltip>
								}
								data-flx="voice.voice-connection-status.voice-details-popout.popout-stat-row"
							/>
						)}
						<PopoutStatRow
							label={i18n._(CURRENT_PING_DESCRIPTOR)}
							value={latency !== null ? formatMilliseconds(latency, locale) : i18n._(MEASURING_DESCRIPTOR)}
							data-flx="voice.voice-connection-status.voice-details-popout.popout-stat-row--2"
						/>
						{averageLatency !== null && averageLatency !== undefined && (
							<PopoutStatRow
								label={i18n._(AVERAGE_PING_DESCRIPTOR)}
								value={formatMilliseconds(averageLatency, locale)}
								data-flx="voice.voice-connection-status.voice-details-popout.popout-stat-row--3"
							/>
						)}
						<PopoutStatRow
							label={i18n._(DURATION_DESCRIPTOR)}
							value={formatDuration(stats.duration)}
							data-flx="voice.voice-connection-status.voice-details-popout.popout-stat-row--4"
						/>
						<PopoutStatRow
							label={i18n._(PARTICIPANTS_DESCRIPTOR)}
							value={participantCount}
							data-flx="voice.voice-connection-status.voice-details-popout.popout-stat-row--5"
						/>
						{strippedEndpoint && (
							<PopoutStatRow
								label={i18n._(ENDPOINT_DESCRIPTOR)}
								value={
									<EndpointCopyBadge
										endpoint={strippedEndpoint}
										i18n={i18n}
										label={i18n._(COPY_ENDPOINT_DESCRIPTOR)}
										data-flx="voice.voice-connection-status.voice-details-popout.endpoint-copy-badge"
									/>
								}
								data-flx="voice.voice-connection-status.voice-details-popout.popout-stat-row--6"
							/>
						)}
					</PopoutStatsSection>
					<AdvancedStatsAccordion
						title={i18n._(ADVANCED_STATS_DESCRIPTOR)}
						data-flx="voice.voice-connection-status.voice-details-popout.advanced-stats-accordion"
					>
						<PopoutStatsSection
							title={i18n._(BANDWIDTH_DESCRIPTOR)}
							data-flx="voice.voice-connection-status.voice-details-popout.popout-stats-section--2"
						>
							<PopoutStatRow
								label={i18n._(SEND_BANDWIDTH_DESCRIPTOR)}
								value={formatBitrateKbps(totalSendBitrate, locale)}
								data-flx="voice.voice-connection-status.voice-details-popout.popout-stat-row--7"
							/>
							<PopoutStatRow
								label={i18n._(RECEIVE_BANDWIDTH_DESCRIPTOR)}
								value={formatBitrateKbps(totalReceiveBitrate, locale)}
								data-flx="voice.voice-connection-status.voice-details-popout.popout-stat-row--8"
							/>
							<PopoutStatRow
								label={i18n._(AUDIO_SEND_BANDWIDTH_DESCRIPTOR)}
								value={formatBitrateKbps(stats.audioSendBitrate, locale)}
								data-flx="voice.voice-connection-status.voice-details-popout.popout-stat-row--9"
							/>
							<PopoutStatRow
								label={i18n._(AUDIO_RECEIVE_BANDWIDTH_DESCRIPTOR)}
								value={formatBitrateKbps(stats.audioRecvBitrate, locale)}
								data-flx="voice.voice-connection-status.voice-details-popout.popout-stat-row--10"
							/>
							<PopoutStatRow
								label={i18n._(VIDEO_SEND_BANDWIDTH_DESCRIPTOR)}
								value={formatBitrateKbps(stats.videoSendBitrate, locale)}
								data-flx="voice.voice-connection-status.voice-details-popout.popout-stat-row--11"
							/>
							<PopoutStatRow
								label={i18n._(VIDEO_RECEIVE_BANDWIDTH_DESCRIPTOR)}
								value={formatBitrateKbps(stats.videoRecvBitrate, locale)}
								data-flx="voice.voice-connection-status.voice-details-popout.popout-stat-row--12"
							/>
						</PopoutStatsSection>
						<PopoutStatsSection
							title={i18n._(NETWORK_DESCRIPTOR)}
							data-flx="voice.voice-connection-status.voice-details-popout.popout-stats-section--3"
						>
							<PopoutStatRow
								label={i18n._(AUDIO_PACKET_LOSS_DESCRIPTOR)}
								value={formatPacketLossPercent(stats.audioPacketLoss, locale)}
								data-flx="voice.voice-connection-status.voice-details-popout.popout-stat-row--13"
							/>
							<PopoutStatRow
								label={i18n._(VIDEO_PACKET_LOSS_DESCRIPTOR)}
								value={formatPacketLossPercent(stats.videoPacketLoss, locale)}
								data-flx="voice.voice-connection-status.voice-details-popout.popout-stat-row--14"
							/>
							<PopoutStatRow
								label={i18n._(JITTER_DESCRIPTOR)}
								value={formatMilliseconds(stats.jitter, locale)}
								data-flx="voice.voice-connection-status.voice-details-popout.popout-stat-row--15"
							/>
							{outgoingCapacity && (
								<PopoutStatRow
									label={i18n._(OUTGOING_CAPACITY_DESCRIPTOR)}
									value={outgoingCapacity}
									data-flx="voice.voice-connection-status.voice-details-popout.popout-stat-row--16"
								/>
							)}
							{incomingCapacity && (
								<PopoutStatRow
									label={i18n._(INCOMING_CAPACITY_DESCRIPTOR)}
									value={incomingCapacity}
									data-flx="voice.voice-connection-status.voice-details-popout.popout-stat-row--17"
								/>
							)}
							{publisherTransportSummary && (
								<PopoutStatRow
									label={i18n._(PUBLISHER_TRANSPORT_DESCRIPTOR)}
									value={
										<Tooltip
											text={publisherTransportSummary}
											data-flx="voice.voice-connection-status.voice-details-popout.tooltip--2"
										>
											<span
												className={styles.popoutStatTextValue}
												data-flx="voice.voice-connection-status.voice-details-popout.publisher-transport-summary"
											>
												{publisherTransportSummary}
											</span>
										</Tooltip>
									}
									data-flx="voice.voice-connection-status.voice-details-popout.popout-stat-row--18"
								/>
							)}
							{subscriberTransportSummary && (
								<PopoutStatRow
									label={i18n._(SUBSCRIBER_TRANSPORT_DESCRIPTOR)}
									value={
										<Tooltip
											text={subscriberTransportSummary}
											data-flx="voice.voice-connection-status.voice-details-popout.tooltip--3"
										>
											<span
												className={styles.popoutStatTextValue}
												data-flx="voice.voice-connection-status.voice-details-popout.subscriber-transport-summary"
											>
												{subscriberTransportSummary}
											</span>
										</Tooltip>
									}
									data-flx="voice.voice-connection-status.voice-details-popout.popout-stat-row--19"
								/>
							)}
						</PopoutStatsSection>
					</AdvancedStatsAccordion>
				</div>
			</Scroller>
		</div>
	);
});
