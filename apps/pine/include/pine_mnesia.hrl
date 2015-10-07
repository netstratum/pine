-record(reference, {
    key,
    values,
    notes,
    created_on,
    created_by,
    modified_on,
    modified_by
  }).

-record(sysconf, {
    key,
    value,
    notes,
    created_on,
    created_by,
    modified_on,
    modified_by
  }).

-record(access, {
    id,
    name,
    notes,
    token,
    limits,
    status,
    created_on,
    modified_on
  }).

-record(roles,{
    id,
    name,
    notes,
    access_tokens,
    access_limits,
    status,
    status_comment,
    created_on,
    created_by,
    modified_on,
    modified_by
  }).

-record(users,{
    id,
    name,
    notes,
    email,
    password,
    access_expiry,
    role,
    role_expiry,
    elevate_comment,
    role_revert,
    status,
    status_comment,
    created_on,
    created_by,
    modified_on,
    modified_by
  }).

-record(sessions, {
    id,
    user,
    source,
    expiry,
    status,
    created_on,
    modified_on
  }).

-record(session_log, {
    id,
    user,
    starttime,
    cookie,
    endtime,
    status,
    created_on
  }).

-record(access_log, {
    id,
    sessionid,
    user,
    request,
    status,
    response,
    timestamp
  }).

-record(api_handlers, {
    function,
    arguments,
    handler,
    statusCodes,
    created_on,
    modified_on
  }).

-record(templates, {
    id,
    name,
    notes,
    label_pattern,
    seq_pattern,
    currency,
    face_value,
    deduction,
    actual_value,
    expiry,
    pin_type,
    pin_pattern,
    attributes,
    batch_size,
    order_limits,
    usage_mode,
    status,
    status_comment,
    created_on,
    created_by,
    modified_on,
    modified_by
  }).

-record(printers, {
    id,
    name,
    notes,
    location,
    crypto_key,
    status,
    status_comment,
    created_on,
    created_by,
    modified_on,
    modified_by
  }).


-record(orders, {
    id,
    name,
    notes,
    imported_flag,
    pin_template,
    label_fillers,
    seq_fillers,
    pin_fillers,
    pin_count,
    batch_count,
    printer,
    crypto_key,
    schedule,
    pins_location,
    status,
    status_comments,
    created_on,
    created_by,
    create_comments,
    approved_on,
    approved_by,
    approve_comments,
    generate_starttime,
    generate_endtime,
    loaded_on,
    loaded_by,
    load_comments,
    activated_on,
    activited_by,
    activate_comments
  }).

-record(pins, {
    id,
    seq,
    pin,
    order,
    value,
    status,
    created_on,
    opened_on,
    opened_by,
    expires_on,
    used_on,
    used_by
  }).

-record(usedpins, {
    id,
    seq,
    pin,
    order,
    value,
    status,
    created_on,
    opened_on,
    opened_by,
    used_on,
    used_by
  }).

