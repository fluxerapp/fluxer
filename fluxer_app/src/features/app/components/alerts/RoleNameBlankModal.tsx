// SPDX-License-Identifier: AGPL-3.0-or-later

import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {GenericErrorModal} from './GenericErrorModal';

const ROLE_NAME_CANNOT_BE_BLANK_DESCRIPTOR = msg({
	message: 'Role name is required',
	comment: 'Validation modal title when the role-edit form is submitted with an empty role name.',
});
const YOU_CANNOT_SAVE_A_ROLE_WITH_A_BLANK_DESCRIPTOR = msg({
	message: 'Give the role a name before saving.',
	comment: 'Modal body shown when the role-edit form is submitted with an empty role name.',
});
export const RoleNameBlankModal = observer(() => {
	const {i18n} = useLingui();
	return (
		<GenericErrorModal
			title={i18n._(ROLE_NAME_CANNOT_BE_BLANK_DESCRIPTOR)}
			message={i18n._(YOU_CANNOT_SAVE_A_ROLE_WITH_A_BLANK_DESCRIPTOR)}
			data-flx="app.role-name-blank-modal.confirm-modal"
		/>
	);
});
