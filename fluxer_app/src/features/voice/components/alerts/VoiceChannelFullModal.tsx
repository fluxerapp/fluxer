// SPDX-License-Identifier: AGPL-3.0-or-later

import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const VOICE_CHANNEL_FULL_DESCRIPTOR = msg({
	message: 'Voice channel full',
	comment: 'Title of the modal shown when a voice channel has reached its participant cap.',
});
const THIS_VOICE_CHANNEL_HAS_REACHED_ITS_USER_LIMIT_DESCRIPTOR = msg({
	message: 'This voice channel is at its user limit. Try again later or join a different channel.',
	comment: 'Body of the voice-channel-full modal.',
});
export const VoiceChannelFullModal = observer(() => {
	const {i18n} = useLingui();
	return (
		<GenericErrorModal
			title={i18n._(VOICE_CHANNEL_FULL_DESCRIPTOR)}
			message={i18n._(THIS_VOICE_CHANNEL_HAS_REACHED_ITS_USER_LIMIT_DESCRIPTOR)}
			data-flx="voice.voice-channel-full-modal.generic-error-modal"
		/>
	);
});
