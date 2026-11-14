// SPDX-License-Identifier: AGPL-3.0-or-later

import {useLayoutEffect} from 'react';

export type StepTitleChangeHandler = (title: string | null) => void;

export function useReportStepTitle(onStepTitleChange: StepTitleChangeHandler | null, title: string | null): void {
	useLayoutEffect(() => {
		onStepTitleChange?.(title);
	}, [onStepTitleChange, title]);
}
