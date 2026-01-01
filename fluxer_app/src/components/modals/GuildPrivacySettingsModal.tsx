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
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import * as UserSettingsActionCreators from '~/actions/UserSettingsActionCreators';
import {Switch} from '~/components/form/Switch';
import * as Modal from '~/components/modals/Modal';
import {Button} from '~/components/uikit/Button/Button';
import GuildStore from '~/stores/GuildStore';
import UserSettingsStore from '~/stores/UserSettingsStore';
import styles from './GuildPrivacySettingsModal.module.css';

export const GuildPrivacySettingsModal = observer(function GuildPrivacySettingsModal({guildId}: {guildId: string}) {
	const {t} = useLingui();
	const guild = GuildStore.getGuild(guildId);
	const restrictedGuilds = UserSettingsStore.restrictedGuilds;

	if (!guild) return null;

	const isDMsAllowed = !restrictedGuilds.includes(guildId);

	const handleToggleDMs = async (value: boolean) => {
		let newRestrictedGuilds: Array<string>;

		if (value) {
			newRestrictedGuilds = restrictedGuilds.filter((id) => id !== guildId);
		} else {
			newRestrictedGuilds = [...restrictedGuilds, guildId];
		}

		await UserSettingsActionCreators.update({
			restrictedGuilds: newRestrictedGuilds,
		});
	};

	return (
		<Modal.Root size="small" centered>
			<Modal.Header title={t`Privacy Settings`} />
			<Modal.Content>
				<div className={styles.container}>
					<Switch
						label={t`Direct Messages`}
						description={t`Allow direct messages from other members in this community`}
						value={isDMsAllowed}
						onChange={handleToggleDMs}
					/>
				</div>
			</Modal.Content>
			<Modal.Footer>
				<Button onClick={() => ModalActionCreators.pop()}>{t`Done`}</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});
