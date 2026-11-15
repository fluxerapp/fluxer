// SPDX-License-Identifier: AGPL-3.0-or-later

import {DebugModal, type DebugTab} from '@app/features/devtools/components/debug/DebugModal';
import {msg} from '@lingui/core/macro';
import {observer} from 'mobx-react-lite';
import {useMemo} from 'react';

export const CHANNEL_RECORD_DESCRIPTOR = msg({
	message: 'Channel record',
	comment: 'Developer debug modal tab showing the raw channel data record.',
});

export const COMMUNITY_RECORD_DESCRIPTOR = msg({
	message: 'Community record',
	comment: 'Developer debug modal tab showing the raw community data record.',
});

export const COMMUNITY_MEMBER_RECORD_DESCRIPTOR = msg({
	message: 'Community member record',
	comment: 'Developer debug modal tab showing the raw community-member data record.',
});

export const USER_RECORD_DESCRIPTOR = msg({
	message: 'User record',
	comment: 'Developer debug modal tab showing the raw user data record.',
});

interface RecordDebugModalProps {
	title: string;
	label: string;
	record: {toJSON(): unknown};
}
export const RecordDebugModal = observer(({title, label, record}: RecordDebugModalProps) => {
	const recordJsonData = useMemo(() => record.toJSON(), [record]);
	const tabs: Array<DebugTab> = [{id: 'record', label, data: recordJsonData}];
	return <DebugModal title={title} tabs={tabs} data-flx="devtools.debug.record-debug-modal.debug-modal" />;
});
