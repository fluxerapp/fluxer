// SPDX-License-Identifier: AGPL-3.0-or-later

import {MATURE_CONTENT_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const GROUP_MATURE_CONTENT_DESCRIPTION_DESCRIPTOR = msg({
	message:
		'Allows media flagged as sensitive in this group. Everyone in the group must be 18 or older. Each member still sees it according to their own sensitive content settings.',
	comment: 'Description under the mature content switch in the edit group settings. Only the group owner sees it.',
});

interface GroupMatureContentSwitchProps {
	value: boolean;
	onChange: (value: boolean) => void;
	'data-flx'?: string;
}

export const GroupMatureContentSwitch = observer(
	({value, onChange, 'data-flx': dataFlx}: GroupMatureContentSwitchProps) => {
		const {i18n} = useLingui();
		return (
			<Switch
				label={i18n._(MATURE_CONTENT_DESCRIPTOR)}
				description={i18n._(GROUP_MATURE_CONTENT_DESCRIPTION_DESCRIPTOR)}
				value={value}
				onChange={onChange}
				data-flx={dataFlx}
			/>
		);
	},
);
