import 'dart:async';

import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:fluxer_app/core/theme/fluxer_theme_extension.dart';
import 'package:fluxer_app/features/events/domain/event.dart';
import 'package:fluxer_app/features/events/providers/events_provider.dart';
import 'package:fluxer_app/features/ui/ui.dart';
import 'package:fluxer_app/l10n/generated/fluxer_localizations.dart';
import 'package:phosphor_flutter/phosphor_flutter.dart';

/// Bottom sheet for creating a new event in a calendar channel.
///
/// Call [showCreateEventSheet] to display it.
Future<GuildEvent?> showCreateEventSheet(
  BuildContext context, {
  required String channelId,
  required DateTime initialDate,
}) {
  return FluxerBottomSheet.showScrollable<GuildEvent?>(
    context,
    title: 'Create Event',
    initialChildSize: 0.9,
    minChildSize: 0.5,
    maxChildSize: 0.95,
    builder: (ctx, controller, close) => _CreateEventSheet(
      channelId: channelId,
      initialDate: initialDate,
      scrollController: controller,
      onClose: close,
    ),
  );
}

class _CreateEventSheet extends ConsumerStatefulWidget {
  const _CreateEventSheet({
    required this.channelId,
    required this.initialDate,
    required this.scrollController,
    required this.onClose,
  });

  final String channelId;
  final DateTime initialDate;
  final ScrollController scrollController;
  final void Function(GuildEvent? result) onClose;

  @override
  ConsumerState<_CreateEventSheet> createState() => _CreateEventSheetState();
}

class _CreateEventSheetState extends ConsumerState<_CreateEventSheet> {
  final _formKey = GlobalKey<FormState>();
  final _nameController = TextEditingController();
  final _descriptionController = TextEditingController();
  final _locationController = TextEditingController();

  late DateTime _startsAt;
  late DateTime _endsAt;
  EventRepeatType _repeatType = EventRepeatType.never;
  bool _isSubmitting = false;

  @override
  void initState() {
    super.initState();
    // Default: starts at next even hour today, ends 1 hour later
    final now = widget.initialDate;
    _startsAt = DateTime(
      now.year,
      now.month,
      now.day,
      now.hour + 1,
    );
    _endsAt = _startsAt.add(const Duration(hours: 1));
  }

  @override
  void dispose() {
    _nameController.dispose();
    _descriptionController.dispose();
    _locationController.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return Form(
      key: _formKey,
      child: ListView(
        controller: widget.scrollController,
        padding: const EdgeInsets.symmetric(horizontal: 16, vertical: 8),
        children: [
          // Event name
          _SectionLabel('Event Name *'),
          const SizedBox(height: 6),
          TextFormField(
            controller: _nameController,
            autofocus: true,
            maxLength: 100,
            decoration: _inputDecoration(context, 'e.g. Gaming Night'),
            validator: (v) =>
                (v == null || v.trim().isEmpty) ? 'Name is required' : null,
            style: TextStyle(color: context.colors.textPrimary),
          ),

          const SizedBox(height: 16),

          // Start date/time
          _SectionLabel('Start'),
          const SizedBox(height: 6),
          _DateTimeRow(
            dateTime: _startsAt,
            onChanged: (dt) {
              setState(() {
                _startsAt = dt;
                if (_endsAt.isBefore(_startsAt)) {
                  _endsAt = _startsAt.add(const Duration(hours: 1));
                }
              });
            },
          ),

          const SizedBox(height: 16),

          // End date/time
          _SectionLabel('End'),
          const SizedBox(height: 6),
          _DateTimeRow(
            dateTime: _endsAt,
            minimumDate: _startsAt,
            onChanged: (dt) => setState(() => _endsAt = dt),
          ),

          const SizedBox(height: 16),

          // Description
          _SectionLabel('Description'),
          const SizedBox(height: 6),
          TextFormField(
            controller: _descriptionController,
            maxLines: 3,
            maxLength: 1000,
            decoration:
                _inputDecoration(context, 'Add event details…'),
            style: TextStyle(color: context.colors.textPrimary),
          ),

          const SizedBox(height: 16),

          // Location
          _SectionLabel('Location'),
          const SizedBox(height: 6),
          TextFormField(
            controller: _locationController,
            maxLength: 255,
            decoration: _inputDecoration(
              context,
              'Channel name or location URL',
            ),
            style: TextStyle(color: context.colors.textPrimary),
          ),

          const SizedBox(height: 16),

          // Repeat
          _SectionLabel('Repeat'),
          const SizedBox(height: 6),
          Container(
            padding: const EdgeInsets.symmetric(horizontal: 12),
            decoration: BoxDecoration(
              color: context.colors.backgroundSecondary,
              borderRadius: BorderRadius.circular(8),
              border: Border.all(color: context.colors.borderColor),
            ),
            child: DropdownButtonHideUnderline(
              child: DropdownButton<EventRepeatType>(
                value: _repeatType,
                dropdownColor: context.colors.backgroundSecondary,
                style: TextStyle(color: context.colors.textPrimary),
                items: EventRepeatType.values
                    .map(
                      (t) => DropdownMenuItem(
                        value: t,
                        child: Text(_repeatLabel(t)),
                      ),
                    )
                    .toList(),
                onChanged: (t) => setState(() => _repeatType = t!),
              ),
            ),
          ),

          const SizedBox(height: 28),

          // Submit button
          FluxerFilledButton(
            onPressed: _isSubmitting ? null : _submit,
            child: _isSubmitting
                ? const SizedBox(
                    width: 20,
                    height: 20,
                    child: CircularProgressIndicator(
                      strokeWidth: 2,
                      color: Colors.white,
                    ),
                  )
                : const Text('Create Event'),
          ),

          const SizedBox(height: 24),
        ],
      ),
    );
  }

  InputDecoration _inputDecoration(BuildContext context, String hint) {
    return InputDecoration(
      hintText: hint,
      hintStyle: TextStyle(color: context.colors.textTertiary),
      filled: true,
      fillColor: context.colors.backgroundSecondary,
      counterStyle: TextStyle(color: context.colors.textTertiary),
      border: OutlineInputBorder(
        borderRadius: BorderRadius.circular(8),
        borderSide: BorderSide(color: context.colors.borderColor),
      ),
      enabledBorder: OutlineInputBorder(
        borderRadius: BorderRadius.circular(8),
        borderSide: BorderSide(color: context.colors.borderColor),
      ),
      focusedBorder: OutlineInputBorder(
        borderRadius: BorderRadius.circular(8),
        borderSide: BorderSide(
          color: context.colors.brandExperiment,
          width: 1.5,
        ),
      ),
    );
  }

  String _repeatLabel(EventRepeatType type) {
    switch (type) {
      case EventRepeatType.never:
        return 'Does not repeat';
      case EventRepeatType.daily:
        return 'Every day';
      case EventRepeatType.weekly:
        return 'Every week';
      case EventRepeatType.monthly:
        return 'Every month';
    }
  }

  Future<void> _submit() async {
    if (!_formKey.currentState!.validate()) return;
    setState(() => _isSubmitting = true);
    try {
      final event = await ref
          .read(channelEventsProvider(widget.channelId).notifier)
          .createEvent(
            name: _nameController.text.trim(),
            startsAt: _startsAt,
            endsAt: _endsAt,
            description: _descriptionController.text.trim().isEmpty
                ? null
                : _descriptionController.text.trim(),
            locationText: _locationController.text.trim().isEmpty
                ? null
                : _locationController.text.trim(),
            repeatType: _repeatType,
          );
      if (mounted) {
        widget.onClose(event);
      }
    } catch (e) {
      if (mounted) {
        ScaffoldMessenger.of(context).showSnackBar(
          SnackBar(
            content: Text('Failed to create event: $e'),
            backgroundColor: context.colors.statusRed,
          ),
        );
      }
    } finally {
      if (mounted) setState(() => _isSubmitting = false);
    }
  }
}

class _SectionLabel extends StatelessWidget {
  const _SectionLabel(this.text);

  final String text;

  @override
  Widget build(BuildContext context) {
    return Text(
      text.toUpperCase(),
      style: context.textStyles.labelSmall.copyWith(
        color: context.colors.textTertiary,
        fontWeight: FontWeight.w700,
        letterSpacing: 0.5,
        fontSize: 11,
      ),
    );
  }
}

class _DateTimeRow extends StatelessWidget {
  const _DateTimeRow({
    required this.dateTime,
    required this.onChanged,
    this.minimumDate,
  });

  final DateTime dateTime;
  final DateTime? minimumDate;
  final ValueChanged<DateTime> onChanged;

  @override
  Widget build(BuildContext context) {
    return Row(
      children: [
        Expanded(
          child: _DateTimeButton(
            label: _dateLabel(dateTime),
            icon: PhosphorIconsRegular.calendarBlank,
            onTap: () => _pickDate(context),
          ),
        ),
        const SizedBox(width: 8),
        Expanded(
          child: _DateTimeButton(
            label: _timeLabel(dateTime),
            icon: PhosphorIconsRegular.clock,
            onTap: () => _pickTime(context),
          ),
        ),
      ],
    );
  }

  String _dateLabel(DateTime dt) {
    const months = [
      'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
      'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec',
    ];
    return '${months[dt.month - 1]} ${dt.day}, ${dt.year}';
  }

  String _timeLabel(DateTime dt) {
    final h = dt.hour % 12 == 0 ? 12 : dt.hour % 12;
    final m = dt.minute.toString().padLeft(2, '0');
    final ampm = dt.hour < 12 ? 'AM' : 'PM';
    return '$h:$m $ampm';
  }

  Future<void> _pickDate(BuildContext context) async {
    final picked = await showDatePicker(
      context: context,
      initialDate: dateTime,
      firstDate: minimumDate ?? DateTime.now(),
      lastDate: DateTime.now().add(const Duration(days: 365 * 5)),
    );
    if (picked != null) {
      onChanged(DateTime(
        picked.year,
        picked.month,
        picked.day,
        dateTime.hour,
        dateTime.minute,
      ));
    }
  }

  Future<void> _pickTime(BuildContext context) async {
    final picked = await showTimePicker(
      context: context,
      initialTime: TimeOfDay.fromDateTime(dateTime),
    );
    if (picked != null) {
      onChanged(DateTime(
        dateTime.year,
        dateTime.month,
        dateTime.day,
        picked.hour,
        picked.minute,
      ));
    }
  }
}

class _DateTimeButton extends StatelessWidget {
  const _DateTimeButton({
    required this.label,
    required this.icon,
    required this.onTap,
  });

  final String label;
  final IconData icon;
  final VoidCallback onTap;

  @override
  Widget build(BuildContext context) {
    return GestureDetector(
      onTap: onTap,
      child: Container(
        padding:
            const EdgeInsets.symmetric(horizontal: 12, vertical: 12),
        decoration: BoxDecoration(
          color: context.colors.backgroundSecondary,
          borderRadius: BorderRadius.circular(8),
          border: Border.all(color: context.colors.borderColor),
        ),
        child: Row(
          children: [
            PhosphorIcon(
              icon,
              size: 16,
              color: context.colors.textTertiary,
            ),
            const SizedBox(width: 8),
            Expanded(
              child: Text(
                label,
                style: TextStyle(
                  color: context.colors.textPrimary,
                  fontSize: 14,
                ),
                overflow: TextOverflow.ellipsis,
              ),
            ),
          ],
        ),
      ),
    );
  }
}
