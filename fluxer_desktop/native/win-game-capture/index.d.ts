// SPDX-License-Identifier: AGPL-3.0-or-later

export declare function elevateGpuSchedulingPriority(processId?: number, priorityClass?: 'high' | 'realtime'): boolean;
export declare function restoreGpuSchedulingPriority(processId?: number): boolean;
export declare const loadError: Error | null;
