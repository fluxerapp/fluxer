// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {Platform} from '@app/features/platform/types/Platform';
import AdaptiveScreenShareEngine from '@app/features/voice/engine/AdaptiveScreenShareEngine';
import {markScreenShareCaptureEnded} from '@app/features/voice/engine/ScreenShareCaptureDiagnostics';
import type {NegotiationReason} from '@app/features/voice/engine/ScreenShareCodecNegotiation';
import ScreenSharePublicationMigration from '@app/features/voice/engine/ScreenSharePublicationMigration';
import {updateLocalParticipantFromRoom} from '@app/features/voice/engine/VoiceMediaEngineBridge';
import {selectScreenShareCodecRepublishDecision} from '@app/features/voice/engine/VoiceScreenShareCodecRepublishPolicy';
import {enforceLocalMediaPublicationCap} from '@app/features/voice/engine/VoiceTrackPublicationUtils';
import {VoiceTrackSource} from '@app/features/voice/engine/VoiceTrackSource';
import type {VoiceEngineV2AppScreenShareExecutionAdapter} from '@app/features/voice/engine/v2/VoiceEngineV2AppScreenShareExecutionAdapter';
import {logger, releaseScreenShareCaptureCleanup} from '@app/features/voice/engine/voice_screen_share_manager/shared';
import {SCREEN_SHARE_DEGRADATION_PREFERENCE} from '@app/features/voice/utils/ScreenShareOptions';
import {
	type LocalParticipant,
	type LocalTrackPublication,
	type LocalVideoTrack,
	type Room,
	type ScreenShareCaptureOptions,
	Track,
	type TrackPublishOptions,
	type VideoCodec,
} from 'livekit-client';

interface MigrationContext {
	readonly room: Room;
	readonly participant: LocalParticipant;
	readonly publication: LocalTrackPublication;
	readonly screenShareTrack: LocalVideoTrack;
	readonly mediaStreamTrack: MediaStreamTrack;
	readonly previousOptions: TrackPublishOptions;
	readonly nextPublishOptions: TrackPublishOptions;
	readonly currentCodec?: VideoCodec;
	readonly codec: VideoCodec;
	readonly reason: NegotiationReason;
	readonly forced: boolean;
	readonly migrationId: string;
	readonly generation: number;
	readonly previousTrackSid: string | null;
	readonly previousTrackName: string | undefined;
}

interface MigrationState {
	candidateMediaStreamTrack: MediaStreamTrack | null;
	candidatePublication: LocalTrackPublication | null;
	candidateTrack: LocalVideoTrack | undefined;
	oldVideoUnpublished: boolean;
	committedTrackSid: string | null;
}

export class VoiceEngineV2AppScreenShareCodecMigration {
	private readonly adapter: VoiceEngineV2AppScreenShareExecutionAdapter;

	constructor(adapter: VoiceEngineV2AppScreenShareExecutionAdapter) {
		this.adapter = adapter;
	}

	private async applyActiveAudioSetting(participant: LocalParticipant, audio: boolean): Promise<boolean> {
		assert.ok(participant);
		assert.equal(typeof audio, 'boolean');
		const screenShareAudioPublication = participant.getTrackPublication(Track.Source.ScreenShareAudio);
		if (!screenShareAudioPublication) {
			if (audio) {
				logger.info('Cannot enable screen share audio without restarting screen share');
			}
			return false;
		}
		try {
			if (audio) {
				await screenShareAudioPublication.unmute();
			} else {
				await screenShareAudioPublication.mute();
			}
			return true;
		} catch (error) {
			logger.warn('Failed to update active screen share audio state', {error, includeAudio: audio});
			return false;
		}
	}

	private async applyActiveResolutionSetting(
		screenShareTrack: LocalVideoTrack,
		resolution: NonNullable<ScreenShareCaptureOptions['resolution']>,
	): Promise<boolean> {
		assert.ok(screenShareTrack);
		assert.ok(resolution);
		const nextConstraints: MediaTrackConstraints = {};
		if (resolution.width > 0) nextConstraints.width = {ideal: resolution.width};
		if (resolution.height > 0) nextConstraints.height = {ideal: resolution.height};
		if (resolution.frameRate !== undefined) {
			nextConstraints.frameRate = {ideal: resolution.frameRate, max: resolution.frameRate};
		}
		try {
			await screenShareTrack.mediaStreamTrack.applyConstraints(nextConstraints);
			return true;
		} catch (error) {
			logger.warn('Failed to update active screen share constraints', {error, resolution});
			return false;
		}
	}

	async updateActiveSettings(
		room: Room | null,
		options?: ScreenShareCaptureOptions,
		publishOptions?: TrackPublishOptions,
	): Promise<boolean> {
		assert.ok(options === undefined || typeof options === 'object');
		if (Platform.OS !== 'web') {
			logger.warn('Screen share updates are not supported on native');
			return false;
		}
		const participant = room?.localParticipant;
		if (!participant || !participant.isScreenShareEnabled) return false;
		const screenSharePublication = participant.getTrackPublication(Track.Source.ScreenShare);
		const screenShareTrack = screenSharePublication?.videoTrack;
		if (!screenShareTrack) {
			logger.warn('No active screen share track to update');
			return false;
		}
		let appliedAnySetting = false;
		if (typeof options?.audio === 'boolean') {
			appliedAnySetting = (await this.applyActiveAudioSetting(participant, options.audio)) || appliedAnySetting;
		}
		if (options?.resolution) {
			appliedAnySetting =
				(await this.applyActiveResolutionSetting(screenShareTrack, options.resolution)) || appliedAnySetting;
		}
		if (options && Object.hasOwn(options, 'contentHint')) {
			screenShareTrack.mediaStreamTrack.contentHint = options.contentHint ?? '';
			appliedAnySetting = true;
		}
		await screenShareTrack.setDegradationPreference(SCREEN_SHARE_DEGRADATION_PREFERENCE);
		await this.adapter.enforceScreenShareSenderParametersInternal(participant, publishOptions);
		this.adapter.ensureScreenShareKeepAliveSinkInternal(participant);
		appliedAnySetting = true;
		updateLocalParticipantFromRoom(room);
		this.adapter.syncLocalScreenShareAudioStateInternal(participant, participant.isScreenShareEnabled);
		return appliedAnySetting;
	}

	private getMigrationPublishOptions(
		publishOptions: TrackPublishOptions | undefined,
		trackName: string | undefined,
	): TrackPublishOptions {
		return {
			...publishOptions,
			source: Track.Source.ScreenShare,
			stream: VoiceTrackSource.ScreenShare,
			...(trackName ? {name: trackName} : {}),
		};
	}

	private getMigrationPublicationTrack(
		publication: LocalTrackPublication,
		role: 'candidate' | 'fallback',
	): {track: LocalVideoTrack; trackSid: string} {
		const track = publication.videoTrack as LocalVideoTrack | undefined;
		if (!track) {
			throw new Error(`screen share codec migration ${role} published without a local video track`);
		}
		const trackSid = publication.trackSid;
		if (!trackSid) {
			throw new Error(`screen share codec migration ${role} published without a track SID`);
		}
		return {track, trackSid};
	}

	private async activateMigrationPublication(args: {
		room: Room;
		participant: LocalParticipant;
		publication: LocalTrackPublication;
		track: LocalVideoTrack;
		publishOptions?: TrackPublishOptions;
		reapplySenderParameters?: boolean;
	}): Promise<void> {
		const {room, participant, publication, track, publishOptions, reapplySenderParameters = false} = args;
		await enforceLocalMediaPublicationCap(participant, VoiceTrackSource.ScreenShare, {
			preferredPublication: publication,
		});
		this.adapter.applyScreenShareContentHintInternal(participant, undefined, track);
		if (reapplySenderParameters) {
			await this.adapter.enforceScreenShareTrackSenderParametersInternal(track, publishOptions);
		}
		this.adapter.bindScreenShareSenderParameterReapplyInternal(participant, publishOptions, track);
		this.adapter.ensureScreenShareKeepAliveSinkInternal(participant, track);
		this.adapter.monitorActiveScreenShareEndInternal(room, participant, track);
		this.adapter.startEncoderVerificationInternal(room, participant, publishOptions?.videoCodec, track);
		AdaptiveScreenShareEngine.start(room);
		this.adapter.applyScreenShareStateInternal(true, {reason: 'server', sendUpdate: false});
		updateLocalParticipantFromRoom(room);
		this.adapter.syncLocalStreamWatchStateInternal(true);
		this.adapter.syncLocalScreenShareAudioStateInternal(participant, true);
	}

	private async commitMigration(ctx: MigrationContext, state: MigrationState, trackSid: string): Promise<void> {
		assert.equal(typeof trackSid, 'string');
		assert.ok(state);
		const commitMessage = {
			migration_id: ctx.migrationId,
			generation: ctx.generation,
			previous_track_sid: ctx.previousTrackSid,
			candidate_track_sid: trackSid,
		};
		await ScreenSharePublicationMigration.publishCommit(ctx.room, commitMessage).catch((error) => {
			logger.warn('Failed to publish screen share codec migration commit', {
				error,
				migrationId: ctx.migrationId,
				trackSid,
			});
		});
		ScreenSharePublicationMigration.markLocalMigrationCommitted(ctx.participant.identity, commitMessage);
		state.committedTrackSid = trackSid;
	}

	private async abortMigration(ctx: MigrationContext, state: MigrationState, abortReason: string): Promise<void> {
		assert.ok(ctx);
		assert.equal(typeof abortReason, 'string');
		if (state.committedTrackSid) return;
		const abortMessage = {
			migration_id: ctx.migrationId,
			generation: ctx.generation,
			candidate_track_sid: state.candidatePublication?.trackSid ?? null,
			reason: abortReason,
		};
		await ScreenSharePublicationMigration.publishAbort(ctx.room, abortMessage).catch((error) => {
			logger.warn('Failed to publish screen share codec migration abort', {
				error,
				migrationId: ctx.migrationId,
				reason: abortReason,
			});
		});
		ScreenSharePublicationMigration.markLocalMigrationAborted(ctx.participant.identity, abortMessage);
	}

	private isPreviousCodecRestorable(ctx: MigrationContext, state: MigrationState): boolean {
		if (!state.oldVideoUnpublished) return false;
		if (ctx.mediaStreamTrack.readyState === 'ended') return false;
		return true;
	}

	private async restorePreviousCodec(
		ctx: MigrationContext,
		state: MigrationState,
		restoreError: unknown,
	): Promise<boolean> {
		assert.ok(ctx);
		assert.ok(state);
		if (!this.isPreviousCodecRestorable(ctx, state)) return false;
		try {
			const effectivePreviousOptions = await this.adapter.getEffectivePublishOptionsInternal(true, ctx.previousOptions);
			const fallbackPublication = await ctx.participant.publishTrack(
				ctx.mediaStreamTrack,
				this.getMigrationPublishOptions(effectivePreviousOptions, ctx.previousTrackName),
			);
			const fallbackTrackInfo = this.getMigrationPublicationTrack(fallbackPublication, 'fallback');
			await this.adapter.enforceScreenShareTrackSenderParametersInternal(
				fallbackTrackInfo.track,
				effectivePreviousOptions,
			);
			await this.commitMigration(ctx, state, fallbackTrackInfo.trackSid);
			await this.activateMigrationPublication({
				room: ctx.room,
				participant: ctx.participant,
				publication: fallbackPublication,
				track: fallbackTrackInfo.track,
				publishOptions: effectivePreviousOptions,
			});
			logger.warn('Restored previous screen share codec after break-before-make migration failed', {
				error: restoreError,
				migrationId: ctx.migrationId,
				previousCodec: ctx.currentCodec,
				failedCodec: ctx.codec,
				reason: ctx.reason,
			});
			return true;
		} catch (fallbackError) {
			logger.warn('Failed to restore previous screen share codec after break-before-make migration failed', {
				error: fallbackError,
				originalError: restoreError,
				migrationId: ctx.migrationId,
				previousCodec: ctx.currentCodec,
				failedCodec: ctx.codec,
			});
			return false;
		}
	}

	private async announceBreakAndUnpublish(ctx: MigrationContext, state: MigrationState): Promise<void> {
		assert.ok(ctx);
		assert.ok(state);
		const publishBreakMessage = {
			migration_id: ctx.migrationId,
			generation: ctx.generation,
			previous_track_sid: ctx.previousTrackSid,
			codec: ctx.codec,
			reason: ctx.reason,
		};
		ScreenSharePublicationMigration.markLocalMigrationBreaking(ctx.participant.identity, publishBreakMessage);
		await ScreenSharePublicationMigration.publishBreak(ctx.room, {
			migrationId: ctx.migrationId,
			generation: ctx.generation,
			previousTrackSid: ctx.previousTrackSid,
			codec: ctx.codec,
			reason: ctx.reason,
		}).catch((error) => {
			logger.warn('Failed to publish screen share codec break-before-make announcement', {
				error,
				migrationId: ctx.migrationId,
				codec: ctx.codec,
				reason: ctx.reason,
			});
		});
		this.adapter.cleanupActiveScreenShareEndListenerInternal();
		this.adapter.applyScreenShareStateInternal(true, {reason: 'server', sendUpdate: false});
		const codecRepublishCleanupSnapshot = this.adapter.getScreenShareSimulcastCleanupSnapshotInternal(
			ctx.screenShareTrack,
		);
		await ctx.participant.unpublishTrack(ctx.screenShareTrack, false);
		state.oldVideoUnpublished = true;
		this.adapter.applyScreenShareStateInternal(true, {reason: 'server', sendUpdate: false});
		await releaseScreenShareCaptureCleanup(codecRepublishCleanupSnapshot);
	}

	private async runMigrationHappyPath(ctx: MigrationContext, state: MigrationState): Promise<void> {
		assert.ok(ctx);
		assert.ok(state);
		const effectivePublishOptions = await this.adapter.getEffectivePublishOptionsInternal(true, ctx.nextPublishOptions);
		state.candidateMediaStreamTrack =
			typeof ctx.mediaStreamTrack.clone === 'function' ? ctx.mediaStreamTrack.clone() : ctx.mediaStreamTrack;
		this.adapter.applyScreenShareContentHintToMediaTrackInternal(state.candidateMediaStreamTrack);
		this.adapter.cancelEncoderVerificationInternal();
		AdaptiveScreenShareEngine.stop();
		await this.announceBreakAndUnpublish(ctx, state);
		state.candidatePublication = await ctx.participant.publishTrack(
			state.candidateMediaStreamTrack,
			this.getMigrationPublishOptions(effectivePublishOptions, ctx.previousTrackName),
		);
		const candidateTrackInfo = this.getMigrationPublicationTrack(state.candidatePublication, 'candidate');
		state.candidateTrack = candidateTrackInfo.track;
		await this.adapter.enforceScreenShareTrackSenderParametersInternal(state.candidateTrack, effectivePublishOptions);
		await this.commitMigration(ctx, state, candidateTrackInfo.trackSid);
		if (state.candidateMediaStreamTrack !== ctx.mediaStreamTrack) {
			this.adapter.stopMediaTrackInternal(ctx.mediaStreamTrack);
		}
		await this.activateMigrationPublication({
			room: ctx.room,
			participant: ctx.participant,
			publication: state.candidatePublication,
			track: state.candidateTrack,
			publishOptions: effectivePublishOptions,
			reapplySenderParameters: true,
		});
		this.adapter.transitionScreenShareLifecycleInternal({
			type: 'share.resolve',
			active: true,
			sourceType: this.adapter.getActiveScreenShareSourceTypeInternal(),
			encoderVerificationScheduled: this.adapter.encoderVerificationTimer != null,
			streamingPriorityHeld: this.adapter.streamingPriorityHeld,
		});
		logger.info('Migrated active screen share codec with break-before-make publication switch', {
			migrationId: ctx.migrationId,
			previousCodec: ctx.currentCodec,
			codec: ctx.codec,
			reason: ctx.reason,
			forced: ctx.forced,
			strategy: 'break-before-make',
		});
	}

	private hasCandidateMediaTrackToStop(ctx: MigrationContext, state: MigrationState): boolean {
		if (state.committedTrackSid) return false;
		if (!state.candidateMediaStreamTrack) return false;
		return state.candidateMediaStreamTrack !== ctx.mediaStreamTrack;
	}

	private async cleanupCandidateAfterFailure(ctx: MigrationContext, state: MigrationState): Promise<void> {
		assert.ok(ctx);
		assert.ok(state);
		if (!state.committedTrackSid && state.candidateTrack) {
			await ctx.participant.unpublishTrack(state.candidateTrack, false).catch((unpublishError) => {
				logger.warn('Failed to unpublish screen share codec migration candidate after abort', {
					error: unpublishError,
					migrationId: ctx.migrationId,
				});
			});
		}
		if (this.hasCandidateMediaTrackToStop(ctx, state)) {
			assert.ok(state.candidateMediaStreamTrack, 'candidateMediaStreamTrack expected after guard');
			this.adapter.stopMediaTrackInternal(state.candidateMediaStreamTrack);
		}
	}

	private async finalizeMigrationFailure(ctx: MigrationContext, state: MigrationState): Promise<void> {
		assert.ok(ctx);
		assert.ok(state);
		await this.abortMigration(ctx, state, state.oldVideoUnpublished ? 'publisher-error' : 'unpublish-error');
		const actual = ctx.participant.isScreenShareEnabled;
		this.adapter.applyScreenShareStateInternal(actual, {forceSync: !actual, reason: 'user', sendUpdate: !actual});
		updateLocalParticipantFromRoom(ctx.room);
		this.adapter.syncLocalStreamWatchStateInternal(actual);
		this.adapter.syncLocalScreenShareAudioStateInternal(ctx.participant, actual);
		if (actual) {
			this.adapter.monitorActiveScreenShareEndInternal(ctx.room, ctx.participant);
			AdaptiveScreenShareEngine.start(ctx.room);
			await this.adapter.enforceScreenShareSenderParametersInternal(ctx.participant, ctx.previousOptions);
			this.adapter.ensureScreenShareKeepAliveSinkInternal(ctx.participant);
		} else {
			await this.adapter.cleanupLingeringScreenShareTracks(ctx.participant);
			markScreenShareCaptureEnded('screen-share-codec-migration-failed');
		}
		this.adapter.transitionScreenShareLifecycleInternal({
			type: 'share.reject',
			active: actual,
			sourceType: actual ? this.adapter.getActiveScreenShareSourceTypeInternal() : null,
		});
	}

	private async handleMigrationFailure(ctx: MigrationContext, state: MigrationState, error: unknown): Promise<boolean> {
		assert.ok(ctx);
		assert.ok(state);
		logger.warn('Failed to migrate active screen share codec with break-before-make', {
			error,
			migrationId: ctx.migrationId,
			codec: ctx.codec,
			reason: ctx.reason,
		});
		await this.cleanupCandidateAfterFailure(ctx, state);
		const shouldAttemptRestore = !state.committedTrackSid;
		if (shouldAttemptRestore && (await this.restorePreviousCodec(ctx, state, error))) {
			this.adapter.transitionScreenShareLifecycleInternal({
				type: 'share.resolve',
				active: true,
				sourceType: this.adapter.getActiveScreenShareSourceTypeInternal(),
				encoderVerificationScheduled: this.adapter.encoderVerificationTimer != null,
				streamingPriorityHeld: this.adapter.streamingPriorityHeld,
			});
			logger.info('Completed screen share codec migration by falling back to previous codec', {
				migrationId: ctx.migrationId,
				previousCodec: ctx.currentCodec,
				failedCodec: ctx.codec,
				reason: ctx.reason,
				forced: ctx.forced,
			});
			return true;
		}
		await this.finalizeMigrationFailure(ctx, state);
		return false;
	}

	private async migrateBreakBeforeMake(args: {
		room: Room;
		participant: LocalParticipant;
		publication: LocalTrackPublication;
		screenShareTrack: LocalVideoTrack;
		mediaStreamTrack: MediaStreamTrack;
		previousOptions: TrackPublishOptions;
		nextPublishOptions: TrackPublishOptions;
		currentCodec?: VideoCodec;
		codec: VideoCodec;
		reason: NegotiationReason;
		forced: boolean;
	}): Promise<boolean> {
		assert.ok(args.room);
		assert.ok(args.participant);
		const ctx: MigrationContext = {
			...args,
			migrationId: ScreenSharePublicationMigration.createMigrationId(),
			generation: ++this.adapter.screenShareMigrationGeneration,
			previousTrackSid: args.publication.trackSid ?? null,
			previousTrackName: args.publication.trackName,
		};
		const state: MigrationState = {
			candidateMediaStreamTrack: null,
			candidatePublication: null,
			candidateTrack: undefined,
			oldVideoUnpublished: false,
			committedTrackSid: null,
		};
		this.adapter.transitionScreenShareLifecycleInternal({type: 'share.codecRepublish'});
		try {
			await this.runMigrationHappyPath(ctx, state);
			return true;
		} catch (error) {
			return this.handleMigrationFailure(ctx, state, error);
		} finally {
			await this.adapter.applyPendingScreenShareRequestsInternal(ctx.room, ctx.participant);
		}
	}

	private async applyCodecRenegotiationNoop(
		participant: LocalParticipant,
		previousOptions: TrackPublishOptions,
		currentCodec: VideoCodec | undefined,
	): Promise<void> {
		assert.ok(participant);
		const effectivePublishOptions = await this.adapter.getEffectivePublishOptionsInternal(true, {
			...previousOptions,
			...(currentCodec ? {videoCodec: currentCodec} : {}),
		});
		await this.adapter.enforceScreenShareSenderParametersInternal(participant, effectivePublishOptions);
	}

	private buildCodecRenegotiationPublishOptions(
		previousOptions: TrackPublishOptions,
		codec: VideoCodec,
		currentCodec: VideoCodec | undefined,
		force: boolean,
	): TrackPublishOptions {
		assert.equal(typeof force, 'boolean');
		const nextPublishOptions: TrackPublishOptions = {...previousOptions, videoCodec: codec};
		const codecChanged = currentCodec !== codec;
		if (force || codecChanged) {
			delete nextPublishOptions.backupCodec;
			delete nextPublishOptions.backupCodecPolicy;
			delete nextPublishOptions.scalabilityMode;
			delete nextPublishOptions.simulcast;
		}
		return nextPublishOptions;
	}

	async renegotiateActiveCodec(
		room: Room | null,
		codec: VideoCodec,
		reason: NegotiationReason,
		options: {force?: boolean} = {},
	): Promise<boolean> {
		assert.equal(typeof codec, 'string');
		assert.ok(reason !== undefined, 'reason required');
		if (Platform.OS !== 'web') return false;
		const participant = room?.localParticipant;
		if (!room) return false;
		if (!participant) return false;
		if (!participant.isScreenShareEnabled) return false;
		if (this.adapter.isScreenSharePending) {
			this.adapter.queuePendingCodecRepublishRequestInternal(codec, reason, options);
			return false;
		}
		await enforceLocalMediaPublicationCap(participant, VoiceTrackSource.ScreenShare);
		const publication = participant.getTrackPublication(Track.Source.ScreenShare);
		const screenShareTrack = publication?.videoTrack as LocalVideoTrack | undefined;
		if (!publication) return false;
		if (!screenShareTrack) return false;
		if (screenShareTrack.mediaStreamTrack.readyState === 'ended') return false;
		const mediaStreamTrack = screenShareTrack.mediaStreamTrack;
		const previousOptions = ((publication as {options?: TrackPublishOptions}).options ?? {}) as TrackPublishOptions;
		const currentCodec = screenShareTrack.codec ?? previousOptions.videoCodec;
		const decision = selectScreenShareCodecRepublishDecision({
			currentCodec,
			nextCodec: codec,
			reason,
			force: options.force,
		});
		if (decision.action === 'noop') {
			await this.applyCodecRenegotiationNoop(participant, previousOptions, currentCodec);
			return false;
		}
		if (decision.action === 'defer') {
			this.adapter.deferActiveCodecRepublishRequestInternal(codec, reason, options);
			logger.info('Deferring negotiated screen share codec change until the next share start', {
				currentCodec,
				codec,
				reason,
			});
			return false;
		}
		const nextPublishOptions = this.buildCodecRenegotiationPublishOptions(
			previousOptions,
			codec,
			currentCodec,
			options.force === true,
		);
		if (currentCodec === codec && !options.force) {
			const effectivePublishOptions = await this.adapter.getEffectivePublishOptionsInternal(true, nextPublishOptions);
			await this.adapter.enforceScreenShareSenderParametersInternal(participant, effectivePublishOptions);
			return false;
		}
		return this.migrateBreakBeforeMake({
			room,
			participant,
			publication,
			screenShareTrack,
			mediaStreamTrack,
			previousOptions,
			nextPublishOptions,
			currentCodec,
			codec,
			reason,
			forced: options.force === true,
		});
	}
}
