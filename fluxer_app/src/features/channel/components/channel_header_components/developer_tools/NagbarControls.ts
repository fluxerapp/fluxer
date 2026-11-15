// SPDX-License-Identifier: AGPL-3.0-or-later

import type {NagbarControlDefinition} from '@app/features/devtools/components/NagbarControls';
import * as NagbarCommands from '@app/features/ui/commands/NagbarCommands';
import Nagbar, {type NagbarToggleKey} from '@app/features/ui/state/Nagbar';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const USE_ACTUAL_DESCRIPTOR = msg({
	message: 'Use actual',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const FORCE_SHOW_DESCRIPTOR = msg({
	message: 'Force show',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const FORCE_HIDE_DESCRIPTOR = msg({
	message: 'Force hide',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
export const NAGBAR_OVERRIDES_DESCRIPTOR = msg({
	message: 'Nagbar overrides',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const setNagbarForceShow = (control: NagbarControlDefinition): void => {
	NagbarCommands.dismissNagbar(control.forceKey);
	NagbarCommands.setForceHideNagbar(control.forceHideKey, false);
};
export const setNagbarUseActual = (control: NagbarControlDefinition): void => {
	control.resetKeys.forEach((key: NagbarToggleKey) => NagbarCommands.resetNagbar(key));
	NagbarCommands.setForceHideNagbar(control.forceHideKey, false);
};
const setNagbarForceHide = (control: NagbarControlDefinition): void => {
	NagbarCommands.setForceHideNagbar(control.forceHideKey, true);
	NagbarCommands.resetNagbar(control.forceKey);
};
export const getNagbarActionItems = (
	control: NagbarControlDefinition,
): Array<{key: string; label: MessageDescriptor; disabled?: boolean; onClick: () => void}> => [
	{
		key: 'use-actual',
		label: USE_ACTUAL_DESCRIPTOR,
		disabled: control.useActualDisabled?.(Nagbar),
		onClick: () => setNagbarUseActual(control),
	},
	{
		key: 'force-show',
		label: FORCE_SHOW_DESCRIPTOR,
		disabled: control.forceShowDisabled?.(Nagbar),
		onClick: () => setNagbarForceShow(control),
	},
	{
		key: 'force-hide',
		label: FORCE_HIDE_DESCRIPTOR,
		disabled: control.forceHideDisabled?.(Nagbar),
		onClick: () => setNagbarForceHide(control),
	},
];
