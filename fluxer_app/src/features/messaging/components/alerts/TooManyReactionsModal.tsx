// SPDX-License-Identifier: AGPL-3.0-or-later

import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const WHOA_THIS_IS_HEAVY_DESCRIPTOR = msg({
	message: 'Whoa, this is heavy',
	comment: 'Label in the too many reactions modal.',
});
const THIS_IS_ONE_HEAVY_MESSAGE_SOME_REACTIONS_NEED_DESCRIPTOR = msg({
	message: 'This is one heavy message. Some reactions need to be removed before you can add more.',
	comment: 'Description text in the too many reactions modal. Keep the tone plain and specific.',
});
export const TooManyReactionsModal = observer(() => {
	const {i18n} = useLingui();
	return (
		<GenericErrorModal
			title={i18n._(WHOA_THIS_IS_HEAVY_DESCRIPTOR)}
			message={i18n._(THIS_IS_ONE_HEAVY_MESSAGE_SOME_REACTIONS_NEED_DESCRIPTOR)}
			data-flx="messaging.too-many-reactions-modal.confirm-modal"
		/>
	);
});
