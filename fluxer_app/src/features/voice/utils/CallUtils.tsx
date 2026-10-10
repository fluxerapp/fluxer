// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import Channels from '@app/features/channel/state/Channels';
import {
	START_VIDEO_CALL_DESCRIPTOR,
	START_VOICE_CALL_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as CallCommands from '@app/features/voice/commands/CallCommands';
import {CallNotRingableModal} from '@app/features/voice/components/alerts/CallNotRingableModal';
import {VoiceP2pConsentModal} from '@app/features/voice/components/alerts/VoiceP2pConsentModal';
import VoiceP2pConsent from '@app/features/voice/state/VoiceP2pConsent';
import VoiceP2pRollout from '@app/features/voice/state/VoiceP2pRollout';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const ARE_YOU_SURE_YOU_WANT_TO_START_A_DESCRIPTOR = msg({
	message: 'Start a video call?',
	comment: 'Confirm dialog body before starting a 1:1 video call.',
});
const ARE_YOU_SURE_YOU_WANT_TO_START_A_2_DESCRIPTOR = msg({
	message: 'Start a voice call?',
	comment: 'Confirm dialog body before starting a 1:1 voice call.',
});
const logger = new Logger('CallUtils');

export interface CallStartConfirmationOptions {
	bypassConfirm?: boolean;
	kind?: 'voice' | 'video';
	showShiftBypassConfirmationTip?: boolean;
}

export interface CallStartConfirmationEvent {
	shiftKey?: boolean;
}

function shouldBypassCallStartConfirmation(event?: CallStartConfirmationEvent | null): boolean {
	return Boolean(event?.shiftKey);
}

export function getCallStartRequestOptions(
	event?: CallStartConfirmationEvent | null,
	options: Pick<CallStartConfirmationOptions, 'kind'> = {},
): CallStartConfirmationOptions {
	return {
		...options,
		bypassConfirm: shouldBypassCallStartConfirmation(event),
		showShiftBypassConfirmationTip: true,
	};
}

async function checkAndStartCall(channelId: string): Promise<boolean> {
	try {
		const {ringable} = await CallCommands.checkCallEligibility(channelId);
		if (!ringable) {
			ModalCommands.push(
				modal(() => <CallNotRingableModal data-flx="voice.call-utils.check-and-start-call.call-not-ringable-modal" />),
			);
			return false;
		}
		CallCommands.startCall(channelId);
		return true;
	} catch (error) {
		logger.error('Failed to check call eligibility:', error);
		return false;
	}
}

function canOfferP2pCall(channelId: string): boolean {
	if (!VoiceP2pRollout.enabled) return false;
	const channel = Channels.getChannel(channelId);
	return channel != null && channel.recipientIds.length + 1 <= VoiceP2pRollout.maxParticipants;
}

export async function requestStartCall(
	i18n: I18n,
	channelId: string,
	options: CallStartConfirmationOptions = {},
): Promise<void> {
	const {bypassConfirm = false, kind = 'voice', showShiftBypassConfirmationTip = false} = options;
	if (!bypassConfirm && canOfferP2pCall(channelId)) {
		ModalCommands.push(
			modal(() => (
				<VoiceP2pConsentModal
					intent="start"
					allowStandard
					onP2p={async () => {
						VoiceP2pConsent.agree(channelId);
						if (!(await checkAndStartCall(channelId))) VoiceP2pConsent.revoke(channelId);
					}}
					onStandard={() => {
						VoiceP2pConsent.revoke(channelId);
						void checkAndStartCall(channelId);
					}}
					onCancel={() => {}}
					data-flx="voice.call-utils.request-start-call.voice-p2p-consent-modal"
				/>
			)),
		);
		return;
	}
	VoiceP2pConsent.revoke(channelId);
	const shouldConfirm = Accessibility.confirmBeforeStartingCalls && !bypassConfirm;
	if (!shouldConfirm) {
		await checkAndStartCall(channelId);
		return;
	}
	const isVideoCall = kind === 'video';
	const getTitle = () => (isVideoCall ? i18n._(START_VIDEO_CALL_DESCRIPTOR) : i18n._(START_VOICE_CALL_DESCRIPTOR));
	ModalCommands.push(
		modal(() => (
			<ConfirmModal
				title={getTitle()}
				description={
					isVideoCall
						? i18n._(ARE_YOU_SURE_YOU_WANT_TO_START_A_DESCRIPTOR)
						: i18n._(ARE_YOU_SURE_YOU_WANT_TO_START_A_2_DESCRIPTOR)
				}
				primaryText={getTitle()}
				primaryVariant="primary"
				onPrimary={async () => {
					await checkAndStartCall(channelId);
				}}
				showShiftBypassConfirmationTip={showShiftBypassConfirmationTip}
				data-flx="voice.call-utils.request-start-call.confirm-modal"
			/>
		)),
	);
}
