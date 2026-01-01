/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo} from 'react';
import type {ChannelRecord} from '~/records/ChannelRecord';
import {DebugModal, type DebugTab} from './DebugModal';

interface ChannelDebugModalProps {
	title: string;
	channel: ChannelRecord;
}

export const ChannelDebugModal: React.FC<ChannelDebugModalProps> = observer(({title, channel}) => {
	const {t} = useLingui();
	const recordJsonData = useMemo(() => channel.toJSON(), [channel]);

	const tabs: Array<DebugTab> = [
		{
			id: 'record',
			label: t`Channel Record`,
			data: recordJsonData,
		},
	];

	return <DebugModal title={title} tabs={tabs} />;
});
