def new_remote_control_car:
  # Populate the object with the required attributes
  {
    "battery_percentage": 100,
    "distance_driven_in_meters": 0,
    "nickname": null
  }
;

def new_remote_control_car(nickname):
  # Populate the object with the required attributes
  new_remote_control_car | .nickname = nickname
;

def display_distance:
  # Implement the required output string
  "\(.distance_driven_in_meters) meters"
;

def display_battery:
  # Implement the required output string
  if .battery_percentage > 0
  then "Battery at \(.battery_percentage)%"
  else "Battery empty"
  end
;

def drive:
  # Update the input's attributes as required
  if .battery_percentage > 0
  then
    .distance_driven_in_meters += 20
    | .battery_percentage -= 1
  end
;
