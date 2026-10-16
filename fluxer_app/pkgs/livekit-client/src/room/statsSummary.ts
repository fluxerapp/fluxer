// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
type Summary = Record<string, unknown>;

function compact<T extends Summary>(summary: T): Summary {
	const compacted: Summary = {};
	for (const [key, value] of Object.entries(summary)) {
		if (value !== undefined) {
			compacted[key] = value;
		}
	}
	return compacted;
}

function resolution(width?: number, height?: number): string | undefined {
	return width && height ? `${width}x${height}` : undefined;
}

function round(seconds: number): number {
	return Math.round(seconds * 10_000) / 10_000;
}

function jitterBuffer(stat: Summary): number | undefined {
	const delay = stat.jitterBufferDelay as number | undefined;
	const emitted = stat.jitterBufferEmittedCount as number | undefined;
	return delay !== undefined && emitted ? round(delay / emitted) : undefined;
}

function playoutDelay(stat: Summary, playout?: Summary): number | undefined {
	if (stat.playoutDelay !== undefined) {
		return round(stat.playoutDelay as number);
	}
	const total = playout?.totalPlayoutDelay as number | undefined;
	const samples = playout?.totalSamplesCount as number | undefined;
	return total !== undefined && samples ? round(total / samples) : undefined;
}

export function summarizeStatsReport(report: RTCStatsReport) {
	const byId = new Map<string, Summary>();
	const candidatePairs: Array<Summary> = [];
	const inbound: Array<Summary> = [];
	const outbound: Array<Summary> = [];
	let transport: Summary | undefined;

	report.forEach((stat) => byId.set(stat.id, stat));

	const codecOf = (stat: Summary) => (stat.codecId ? byId.get(stat.codecId as string)?.mimeType : undefined);
	const relatedOf = (stat: Summary, key: 'remoteId' | 'mediaSourceId' | 'playoutId') =>
		stat[key] ? byId.get(stat[key] as string) : undefined;

	report.forEach((stat) => {
		switch (stat.type) {
			case 'inbound-rtp': {
				const playout = relatedOf(stat, 'playoutId');
				inbound.push(
					compact({
						kind: stat.kind,
						ssrc: stat.ssrc,
						mid: stat.mid,
						trackId: stat.trackIdentifier,
						codec: codecOf(stat),
						decoder: stat.decoderImplementation,
						resolution: resolution(stat.frameWidth, stat.frameHeight),
						fps: stat.framesPerSecond,
						bytesReceived: stat.bytesReceived,
						packetsReceived: stat.packetsReceived,
						packetsLost: stat.packetsLost,
						packetsDiscarded: stat.packetsDiscarded,
						framesReceived: stat.framesReceived,
						framesDecoded: stat.framesDecoded,
						framesDropped: stat.framesDropped,
						keyFramesDecoded: stat.keyFramesDecoded,
						freezeCount: stat.freezeCount,
						totalFreezesDuration: stat.totalFreezesDuration,
						pauseCount: stat.pauseCount,
						nackCount: stat.nackCount,
						pliCount: stat.pliCount,
						firCount: stat.firCount,
						jitter: stat.jitter,
						jitterBuffer: jitterBuffer(stat),
						playoutDelay: playoutDelay(stat, playout),
						audioLevel: stat.audioLevel,
						totalSamplesReceived: stat.totalSamplesReceived,
						concealedSamples: stat.concealedSamples,
					}),
				);
				break;
			}
			case 'outbound-rtp': {
				const remote = relatedOf(stat, 'remoteId');
				const source = relatedOf(stat, 'mediaSourceId');
				outbound.push(
					compact({
						kind: stat.kind,
						ssrc: stat.ssrc,
						mid: stat.mid,
						rid: stat.rid,
						trackId: source?.trackIdentifier,
						active: stat.active,
						codec: codecOf(stat),
						encoder: stat.encoderImplementation,
						resolution: resolution(stat.frameWidth, stat.frameHeight),
						fps: stat.framesPerSecond,
						captureResolution: resolution(source?.width as number | undefined, source?.height as number | undefined),
						captureFps: source?.framesPerSecond,
						audioLevel: source?.audioLevel,
						targetBitrate: stat.targetBitrate,
						bytesSent: stat.bytesSent,
						packetsSent: stat.packetsSent,
						retransmittedPacketsSent: stat.retransmittedPacketsSent,
						framesEncoded: stat.framesEncoded,
						keyFramesEncoded: stat.keyFramesEncoded,
						limitedBy: stat.qualityLimitationReason === 'none' ? undefined : stat.qualityLimitationReason,
						nackCount: stat.nackCount,
						pliCount: stat.pliCount,
						firCount: stat.firCount,
						remotePacketsLost: remote?.packetsLost,
						remoteFractionLost: remote?.fractionLost,
						remoteJitter: remote?.jitter,
						remoteRoundTripTime: remote?.roundTripTime,
					}),
				);
				break;
			}
			case 'transport':
				transport = stat;
				break;
			case 'candidate-pair':
				candidatePairs.push(stat);
				break;
			default:
		}
	});

	const selectedPairId = transport?.selectedCandidatePairId as string | undefined;
	const pair =
		(selectedPairId ? byId.get(selectedPairId) : undefined) ??
		candidatePairs.find((candidate) => candidate.selected) ??
		candidatePairs.find((candidate) => candidate.nominated);
	const local = pair?.localCandidateId ? byId.get(pair.localCandidateId as string) : undefined;
	const remote = pair?.remoteCandidateId ? byId.get(pair.remoteCandidateId as string) : undefined;

	const connection = compact({
		ice: transport?.iceState,
		dtls: transport?.dtlsState,
		route: local && remote ? `${local.candidateType}/${local.protocol} -> ${remote.candidateType}` : undefined,
		network: local?.networkType,
		currentRoundTripTime: pair?.currentRoundTripTime,
		availableOutgoingBitrate: pair?.availableOutgoingBitrate,
		availableIncomingBitrate: pair?.availableIncomingBitrate,
		bytesSent: pair?.bytesSent,
		bytesReceived: pair?.bytesReceived,
		candidatePairChanges: transport?.selectedCandidatePairChanges,
	});

	return {
		connection: Object.keys(connection).length > 0 ? connection : undefined,
		outbound: outbound.length > 0 ? outbound : undefined,
		inbound: inbound.length > 0 ? inbound : undefined,
	};
}
