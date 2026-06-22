// SPDX-License-Identifier: AGPL-3.0-or-later

import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {GenericErrorModal} from './GenericErrorModal';

const FEATURE_TEMPORARILY_DISABLED_DESCRIPTOR = msg({
	message: 'Feature temporarily disabled',
	comment: 'Short label in the feature temporarily disabled modal.',
});
const THIS_FEATURE_HAS_BEEN_TEMPORARILY_DISABLED_PLEASE_TRY_DESCRIPTOR = msg({
	message: 'This feature is paused. Try again later.',
	comment: 'Modal body shown when an instance admin has temporarily disabled a feature. Keep plain.',
});
export const FeatureTemporarilyDisabledModal = observer(() => {
	const {i18n} = useLingui();
	return (
		<GenericErrorModal
			title={i18n._(FEATURE_TEMPORARILY_DISABLED_DESCRIPTOR)}
			message={i18n._(THIS_FEATURE_HAS_BEEN_TEMPORARILY_DISABLED_PLEASE_TRY_DESCRIPTOR)}
			data-flx="app.feature-temporarily-disabled-modal.confirm-modal"
		/>
	);
});
