// SPDX-License-Identifier: AGPL-3.0-or-later

import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const SCREEN_SHARING_FAILED_DESCRIPTOR = msg({
	message: 'Screen sharing failed',
	comment: 'Title of the modal shown when a screen share attempt fails due to a video source error.',
});
const SCREEN_SHARING_FAILED_BODY_DESCRIPTOR = msg({
	message:
		'Could not start the screen share. The selected source may no longer be available or may have blocked capture. Try sharing your entire screen or monitor instead of a specific window.',
	comment:
		'Body of the modal shown when a screen share attempt fails. Advises the user to try sharing the full screen.',
});
export const ScreenShareFailedModal = observer(() => {
	const {i18n} = useLingui();
	return (
		<GenericErrorModal
			title={i18n._(SCREEN_SHARING_FAILED_DESCRIPTOR)}
			message={i18n._(SCREEN_SHARING_FAILED_BODY_DESCRIPTOR)}
			data-flx="voice.screen-share-failed-modal.confirm-modal"
		/>
	);
});
