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
import * as NagbarActionCreators from '~/actions/NagbarActionCreators';
import {Button} from '~/components/uikit/Button/Button';
import type {NagbarStore, NagbarToggleKey} from '~/stores/NagbarStore';
import styles from './NagbarsTab.module.css';
import {getNagbarControls, type NagbarControlDefinition} from './nagbarControls';

interface NagbarsTabContentProps {
	nagbarState: NagbarStore;
}

const NagbarRow: React.FC<{
	control: NagbarControlDefinition;
	nagbarState: NagbarStore;
}> = observer(({control, nagbarState}) => {
	const {t} = useLingui();
	const getFlag = (key: NagbarToggleKey): boolean => Boolean(nagbarState[key]);
	const useActualDisabled = control.useActualDisabled?.(nagbarState) ?? control.resetKeys.every((key) => !getFlag(key));
	const forceShowDisabled = control.forceShowDisabled?.(nagbarState) ?? getFlag(control.forceKey);
	const forceHideDisabled = control.forceHideDisabled?.(nagbarState) ?? getFlag(control.forceHideKey);

	const handleUseActual = () => {
		control.resetKeys.forEach((key) => NagbarActionCreators.resetNagbar(key));
		NagbarActionCreators.setForceHideNagbar(control.forceHideKey, false);
	};

	const handleForceShow = () => {
		NagbarActionCreators.dismissNagbar(control.forceKey);
		NagbarActionCreators.setForceHideNagbar(control.forceHideKey, false);
	};

	const handleForceHide = () => {
		NagbarActionCreators.setForceHideNagbar(control.forceHideKey, true);
		NagbarActionCreators.resetNagbar(control.forceKey);
	};

	return (
		<div className={styles.nagbarItem}>
			<div className={styles.nagbarInfo}>
				<span className={styles.nagbarLabel}>{control.label}</span>
				<span className={styles.nagbarStatus}>{control.status(nagbarState)}</span>
			</div>
			<div className={styles.buttonGroup}>
				<Button onClick={handleUseActual} disabled={useActualDisabled}>
					{t`Use Actual`}
				</Button>
				<Button onClick={handleForceShow} disabled={forceShowDisabled}>
					{t`Force Show`}
				</Button>
				<Button onClick={handleForceHide} disabled={forceHideDisabled}>
					{t`Force Hide`}
				</Button>
			</div>
		</div>
	);
});

export const NagbarsTabContent: React.FC<NagbarsTabContentProps> = observer(({nagbarState}) => {
	const {t} = useLingui();
	const nagbarControls = getNagbarControls(t);

	return (
		<div className={styles.nagbarList}>
			{nagbarControls.map((control) => (
				<NagbarRow key={control.key} control={control} nagbarState={nagbarState} />
			))}

			<div className={styles.footer}>
				<Button
					onClick={() => NagbarActionCreators.resetAllNagbars()}
					variant="secondary"
				>{t`Reset All Nagbars`}</Button>
			</div>
		</div>
	);
});
