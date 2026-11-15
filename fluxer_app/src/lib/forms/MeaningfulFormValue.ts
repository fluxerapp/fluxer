// SPDX-License-Identifier: AGPL-3.0-or-later

import type {FieldValues, Path, PathValue, UseFormSetValue} from 'react-hook-form';

function getMeaningfulFormValue<TValue>(
	currentValue: TValue,
	cleanValue: TValue,
	isMeaningfullyDirty: boolean,
): TValue {
	return isMeaningfullyDirty ? currentValue : cleanValue;
}

export function setMeaningfulFormValue<TFormValues extends FieldValues, TName extends Path<TFormValues>>({
	setValue,
	name,
	currentValue,
	cleanValue,
	isMeaningfullyDirty,
	shouldTouch = false,
}: {
	readonly setValue: UseFormSetValue<TFormValues>;
	readonly name: TName;
	readonly currentValue: PathValue<TFormValues, TName>;
	readonly cleanValue: PathValue<TFormValues, TName>;
	readonly isMeaningfullyDirty: boolean;
	readonly shouldTouch?: boolean;
}): void {
	setValue(name, getMeaningfulFormValue(currentValue, cleanValue, isMeaningfullyDirty), {
		shouldDirty: true,
		shouldTouch,
	});
}
