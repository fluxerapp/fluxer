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
import {ColorPickerField} from '~/components/form/ColorPickerField';
import myProfileTabStyles from '../MyProfileTab.module.css';
import styles from './AccentColorPicker.module.css';

interface AccentColorPickerProps {
	value: number;
	onChange: (value: number) => void;
	disabled?: boolean;
	errorMessage?: string;
}

export const AccentColorPicker = observer(({value, onChange, disabled, errorMessage}: AccentColorPickerProps) => {
	const {t} = useLingui();
	return (
		<div>
			<ColorPickerField
				label={t`Accent Color`}
				description={t`Customizes the border and banner color on your profile`}
				descriptionClassName={myProfileTabStyles.inputFooter}
				value={value}
				onChange={onChange}
				disabled={disabled}
				defaultValue={0x4641d9}
			/>
			{errorMessage && <p className={styles.errorMessage}>{errorMessage}</p>}
		</div>
	);
});
