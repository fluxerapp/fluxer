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

import {Trans, useLingui} from '@lingui/react/macro';
import {useRoomContext} from '@livekit/components-react';
import {ArrowsClockwiseIcon, ChartBarIcon, MicrophoneIcon, VideoIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import MediaEngineStore from '~/stores/voice/MediaEngineFacade';
import styles from './VoiceStatsOverlay.module.css';

interface VoiceStatsOverlayProps {
	onClose: () => void;
}

const formatDuration = (seconds: number) => {
	const hours = Math.floor(seconds / 3600);
	const minutes = Math.floor((seconds % 3600) / 60);
	const secs = seconds % 60;

	if (hours > 0) {
		return `${hours}:${minutes.toString().padStart(2, '0')}:${secs.toString().padStart(2, '0')}`;
	}

	return `${minutes}:${secs.toString().padStart(2, '0')}`;
};

const formatBitrate = (kbps: number) => (kbps >= 1000 ? `${(kbps / 1000).toFixed(1)} Mbps` : `${kbps} kbps`);

interface ThresholdClass {
	max: number;
	className: string;
}

const classByThresholds = (value: number, thresholds: ReadonlyArray<ThresholdClass>, fallback: string) => {
	for (const t of thresholds) {
		if (value < t.max) return t.className;
	}
	return fallback;
};

const packetLossThresholds = [
	{max: 1, className: styles.textGreen},
	{max: 3, className: styles.textYellow},
	{max: 5, className: styles.textOrange},
] as const;

const rttThresholds = [
	{max: 50, className: styles.textGreen},
	{max: 100, className: styles.textYellow},
	{max: 150, className: styles.textOrange},
] as const;

interface StatRowProps {
	label: React.ReactNode;
	value: React.ReactNode;
	valueClassName?: string;
}

const StatRow = ({label, value, valueClassName}: StatRowProps) => (
	<div className={styles.statRow}>
		<span className={styles.statLabel}>{label}</span>
		<span className={clsx(styles.statValue, valueClassName)}>{value}</span>
	</div>
);

interface SectionProps {
	icon: React.ReactNode;
	title: React.ReactNode;
	children: React.ReactNode;
}

const Section = ({icon, title, children}: SectionProps) => (
	<div className={styles.section}>
		<div className={styles.sectionHeader}>
			{icon}
			{title}
		</div>
		<div className={styles.sectionContent}>{children}</div>
	</div>
);

const Divider = () => <div className={styles.divider} />;

const VoiceStatsOverlayInner = observer(({onClose}: VoiceStatsOverlayProps) => {
	const {t} = useLingui();
	const stats = MediaEngineStore.voiceStats;

	const packetLossClass = (loss: number) =>
		clsx(styles.statValueColored, classByThresholds(loss, packetLossThresholds, styles.textRed));

	const rttClass = (rtt: number) =>
		clsx(styles.statValueColored, classByThresholds(rtt, rttThresholds, styles.textRed));

	return (
		<div className={styles.overlay}>
			<div className={styles.header}>
				<div className={styles.headerContent}>
					<ChartBarIcon weight="fill" className={styles.iconMedium} style={{color: 'var(--text-primary)'}} />
					<h3 className={styles.title}>
						<Trans>Connection Stats</Trans>
					</h3>
				</div>
				<Tooltip text={t`Close`}>
					<FocusRing offset={-2}>
						<button type="button" className={styles.closeButton} onClick={onClose}>
							<XIcon weight="bold" className={styles.iconSmall} />
						</button>
					</FocusRing>
				</Tooltip>
			</div>

			<div className={styles.content}>
				<div className={styles.section}>
					<StatRow label={<Trans>Duration</Trans>} value={formatDuration(stats.duration)} />
					<StatRow label={<Trans>Participants</Trans>} value={stats.participantCount} />
				</div>

				<Divider />

				<Section icon={<MicrophoneIcon weight="fill" className={styles.iconSmall} />} title={<Trans>Audio</Trans>}>
					<StatRow label={<Trans>Send</Trans>} value={formatBitrate(stats.audioSendBitrate)} />
					<StatRow label={<Trans>Receive</Trans>} value={formatBitrate(stats.audioRecvBitrate)} />
					<StatRow
						label={<Trans>Packet Loss</Trans>}
						value={`${stats.audioPacketLoss}%`}
						valueClassName={packetLossClass(stats.audioPacketLoss)}
					/>
				</Section>

				<Divider />

				<Section icon={<VideoIcon weight="fill" className={styles.iconSmall} />} title={<Trans>Video</Trans>}>
					<StatRow label={<Trans>Send</Trans>} value={formatBitrate(stats.videoSendBitrate)} />
					<StatRow label={<Trans>Receive</Trans>} value={formatBitrate(stats.videoRecvBitrate)} />
					<StatRow
						label={<Trans>Packet Loss</Trans>}
						value={`${stats.videoPacketLoss}%`}
						valueClassName={packetLossClass(stats.videoPacketLoss)}
					/>
				</Section>

				<Divider />

				<Section
					icon={<ArrowsClockwiseIcon weight="fill" className={styles.iconSmall} />}
					title={<Trans>Network</Trans>}
				>
					<StatRow
						label={<Trans>Latency (RTT)</Trans>}
						value={`${stats.rtt} ms`}
						valueClassName={rttClass(stats.rtt)}
					/>
					<StatRow label={<Trans>Jitter</Trans>} value={`${stats.jitter} ms`} />
				</Section>
			</div>

			<div className={styles.dividerTop}>
				<p className={styles.footerText}>
					<Trans>Stats update every second</Trans>
				</p>
			</div>
		</div>
	);
});

export const VoiceStatsOverlay = observer(({onClose}: VoiceStatsOverlayProps) => {
	const room = useRoomContext();
	if (!room) return null;
	return <VoiceStatsOverlayInner onClose={onClose} />;
});
